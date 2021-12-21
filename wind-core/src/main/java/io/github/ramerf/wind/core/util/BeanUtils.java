package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.function.FieldFunction;
import java.beans.FeatureDescriptor;
import java.beans.PropertyDescriptor;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.*;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;
import org.springframework.core.type.ClassMetadata;
import org.springframework.core.type.classreading.CachingMetadataReaderFactory;
import org.springframework.util.ClassUtils;

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toList;
import static org.springframework.util.StringUtils.tokenizeToStringArray;

/**
 * The type Bean utils.
 *
 * @author ramer
 * @since 2019 /12/26
 */
@Slf4j
public final class BeanUtils {
  /** 对象所有(包含父类)私有字段. */
  private static final Map<Class<?>, WeakReference<List<Field>>> PRIVATE_FIELDS_MAP =
      new ConcurrentHashMap<>();
  /** 对象的写入方法. */
  private static final Map<Class<?>, WeakReference<List<Method>>> WRITE_METHOD_MAP =
      new ConcurrentHashMap<>();
  /** lambda和对应的Field. */
  private static final Map<FieldFunction, WeakReference<Field>> LAMBDA_FIELD_MAP =
      new ConcurrentHashMap<>();

  /**
   * Map转Bean.
   *
   * @param <R> the type parameter
   * @param map the map
   * @param clazz the clazz
   * @return the r
   */
  public static <R> R mapToBean(Map<String, Object> map, Class<R> clazz) {
    final R r = initial(clazz);
    Stream.of(clazz.getMethods())
        .filter(o -> o.getName().startsWith("set") && o.getParameterTypes().length == 1)
        .forEach(
            f -> {
              final Object value = map.get(camelToUnderline(methodToProperty(f.getName())));
              try {
                f.setAccessible(true);
                f.invoke(r, value);
              } catch (Exception e) {
                log.info(
                    "mapToBean:跳过类型不匹配的字段[{} {}->{}]",
                    methodToProperty(f.getName()),
                    f.getParameterTypes()[0].getSimpleName(),
                    value.getClass().getSimpleName());
              }
            });
    return r;
  }

  /**
   * 获取对象属性值为空的属性名.
   *
   * @param obj the obj
   * @return the null prop
   */
  public static Set<String> getNullProp(@Nonnull final Object obj) {
    final BeanWrapperImpl wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(propertyName -> wrapper.getPropertyValue(propertyName) == null)
        .collect(Collectors.toSet());
  }

  /**
   * 获取对象属性值不为空的属性名.
   *
   * @param obj the obj
   * @return the non null prop
   */
  public static Set<String> getNonNullProp(Object obj) {
    final BeanWrapper wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(name -> wrapper.getPropertyValue(name) != null)
        .filter(name -> !Objects.equals("class", name))
        .collect(Collectors.toSet());
  }

  /**
   * 获取对象的属性名.
   *
   * @param obj the obj
   * @return the all prop
   */
  public static List<String> getAllProp(@Nonnull final Object obj) {
    return Stream.of(new BeanWrapperImpl(obj).getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(o -> !Objects.equals("class", o))
        .collect(toList());
  }

  /**
   * 获取所有(包含父类)private属性.
   *
   * @param clazz the clazz
   * @param container the container
   * @return the list
   */
  public static List<Field> retrievePrivateFields(
      @Nonnull final Class<?> clazz, @Nonnull final Supplier<List<Field>> container) {
    return Optional.ofNullable(PRIVATE_FIELDS_MAP.get(clazz))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final List<Field> list = getPrivateFields(clazz, container.get());
              PRIVATE_FIELDS_MAP.put(clazz, new WeakReference<>(list));
              return list;
            });
  }

  private static List<Field> getPrivateFields(
      @Nonnull Class<?> clazz, @Nonnull final List<Field> fields) {
    do {
      for (final Field superField : clazz.getDeclaredFields()) {
        // 同名覆盖只保留子类字段
        if (fields.stream().noneMatch(o -> o.getName().equals(superField.getName()))) {
          fields.add(superField);
        }
      }
    } while ((clazz = clazz.getSuperclass()) != null);
    return fields;
  }

  /**
   * Gets write methods.
   *
   * @param clazz the clazz
   * @return the write methods
   */
  public static List<Method> getWriteMethods(final Class<?> clazz) {
    return Optional.ofNullable(WRITE_METHOD_MAP.get(clazz))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final BeanWrapperImpl wrapper = new BeanWrapperImpl(clazz);
              final List<Method> methods =
                  Arrays.stream(wrapper.getPropertyDescriptors())
                      .filter(o -> wrapper.isWritableProperty(o.getName()))
                      .map(PropertyDescriptor::getWriteMethod)
                      .collect(toList());
              WRITE_METHOD_MAP.put(clazz, new WeakReference<>(methods));
              return methods;
            });
  }

  /**
   * 实例化对象 .
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the t
   */
  public static <T> T initial(final Class<T> clazz) {
    try {
      return clazz.newInstance();
    } catch (InstantiationException | IllegalAccessException e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw new CommonException(String.format("无法实例化对象[%s]", clazz.getSimpleName()));
  }

  /**
   * 实例化对象.
   *
   * @param <T> the type parameter
   * @param classPath the class path
   * @return the t
   */
  public static <T> T initial(final String classPath) {
    try {
      return initial(getClazz(classPath));
    } catch (Exception e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw new CommonException(String.format("无法实例化对象[%s]", classPath));
  }

  /**
   * 通过类路径获取Class.
   *
   * @param <T> the type parameter
   * @param classPath the class path
   * @return the clazz
   */
  @SuppressWarnings("all")
  public static <T> Class<T> getClazz(String classPath) {
    if (classPath.contains("/")) {
      classPath = classPath.replaceAll("/", ".");
    }
    try {
      // 后期需要改成多个加载器,参考: org.apache.ibatis.io.ClassLoaderWrapper#classForName(java.lang.String,
      // java.lang.ClassLoader[])
      return (Class<T>) Class.forName(classPath);
    } catch (Exception e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw new CommonException(String.format("无法获取class[%s]", classPath));
  }

  /**
   * 通过方法名获取bean属性名.
   *
   * @param name the name
   * @return the string
   */
  public static String methodToProperty(String name) {
    final String is = "is";
    final String get = "get";
    final String set = "set";
    if (name.startsWith(is)) {
      name = name.substring(2);
    } else {
      if (name.startsWith(get) || name.startsWith(set)) {
        name = name.substring(3);
      } else {
        throw new IllegalArgumentException(
            "Error parsing property name '" + name + "'.  Didn't start with 'is', 'get' or 'set'.");
      }
    }
    if (name.length() == 1 || (name.length() > 1 && !Character.isUpperCase(name.charAt(1)))) {
      name = name.substring(0, 1).toLowerCase(Locale.ENGLISH) + name.substring(1);
    }

    return name;
  }

  /**
   * 执行{@link Class#getField(String)},失败抛出 {@link IllegalArgumentException}.
   *
   * @param clazz the clazz
   * @param name the field name
   * @return the optional
   */
  public static Field getDeclaredField(@Nonnull Class<?> clazz, final String name) {
    do {
      try {
        return clazz.getDeclaredField(name);
      } catch (NoSuchFieldException ignored) {
      }
    } while ((clazz = clazz.getSuperclass()) != null);
    return null;
  }

  /**
   * 获取指定包下,指定接口/类的子类.
   *
   * @param <T> the type parameter
   * @param packagePatterns 支持多个包名分隔符: ",; \t\n"
   * @param assignableType 父接口/类
   * @return 所有子类 set
   * @throws IOException the IOException
   */
  public static <T> Set<Class<? extends T>> scanClasses(
      String packagePatterns, Class<T> assignableType) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
    String[] packagePatternArray =
        tokenizeToStringArray(
            packagePatterns, ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS);
    for (String packagePattern : packagePatternArray) {
      Resource[] resources =
          new PathMatchingResourcePatternResolver()
              .getResources(
                  ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX
                      .concat(ClassUtils.convertClassNameToResourcePath(packagePattern))
                      .concat("/**/*.class"));
      for (Resource resource : resources) {
        try {
          ClassMetadata classMetadata =
              new CachingMetadataReaderFactory().getMetadataReader(resource).getClassMetadata();
          Class<T> clazz = BeanUtils.getClazz(classMetadata.getClassName());
          if (assignableType == null || assignableType.isAssignableFrom(clazz)) {
            classes.add(clazz);
          }
        } catch (Throwable e) {
          log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", resource, e.toString());
        }
      }
    }
    return classes;
  }

  /**
   * 获取指定包下,指定接口/类的子类.
   *
   * @param <T> the type parameter
   * @param packagePatterns 支持多个包名分隔符: ",; \t\n"
   * @param annotation 父接口/类
   * @return 所有子类 set
   * @throws IOException the IOException
   */
  public static <T> Set<Class<? extends T>> scanClassesWithAnnotation(
      String packagePatterns, Class<? extends Annotation> annotation) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
    String[] packagePatternArray =
        tokenizeToStringArray(
            packagePatterns, ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS);
    for (String packagePattern : packagePatternArray) {
      Resource[] resources =
          new PathMatchingResourcePatternResolver()
              .getResources(
                  ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX
                      .concat(ClassUtils.convertClassNameToResourcePath(packagePattern))
                      .concat("/**/*.class"));
      for (Resource resource : resources) {
        try {
          ClassMetadata classMetadata =
              new CachingMetadataReaderFactory().getMetadataReader(resource).getClassMetadata();
          Class<T> clazz = BeanUtils.getClazz(classMetadata.getClassName());
          if (annotation == null || clazz.isAnnotationPresent(annotation)) {
            classes.add(clazz);
          }
        } catch (Throwable e) {
          log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", resource, e.toString());
        }
      }
    }
    return classes;
  }

  /**
   * 对于 {@link Method#invoke(Object, Object...)}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.invoke(null, String.class.getMethods()[0], "string")
   *         .ifPresent(e -&gt; log.info(" BeanUtils.main:调用失败处理[{}]", e.getClass()));
   * </pre>
   *
   * @param obj the obj
   * @param method the method
   * @param value the value
   * @return the optional
   */
  public static Optional<Exception> invoke(Object obj, Method method, Object... value) {
    try {
      if (!method.isAccessible()) {
        method.setAccessible(true);
      }
      method.invoke(obj, value);
      return Optional.empty();
    } catch (Exception e) {
      return Optional.of(e);
    }
  }

  /**
   * 对于 {@link Field#get(Object)}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.invoke(obj, field, e -&gt; throw e);
   * </pre>
   *
   * @param obj the obj
   * @param field the field
   * @param consumer 异常时的处理,默认返回null
   * @return the optional
   */
  public static Object getValue(
      final Object obj, final Field field, final Function<Exception, Object> consumer) {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      return field.get(obj);
    } catch (Exception e) {
      return Optional.ofNullable(consumer)
          .map(ex -> ex.apply(e))
          .orElseGet(
              () -> {
                log.warn(e.getMessage(), e);
                return null;
              });
    }
  }

  /**
   * 对于 {@link Field#set(Object, Object)}}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.setValue(obj, field, value, e -&gt; throw e);
   * </pre>
   *
   * @param obj the obj
   * @param field the field
   * @param value 参数
   * @param consumer 异常时的处理,默认返回null
   */
  public static void setValue(
      final Object obj, final Field field, final Object value, Consumer<Exception> consumer) {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      field.set(obj, value);
    } catch (Exception e) {
      if (consumer != null) {
        consumer.accept(e);
      } else {
        throw new CommonException(e);
      }
    }
  }

  /** 基本类型零值. */
  public static Object getPrimitiveDefaultValue(final Class<?> clazz) {
    if (byte.class.equals(clazz)) {
      return 0;
    }
    if (short.class.equals(clazz)) {
      return 0;
    }
    if (int.class.equals(clazz)) {
      return 0;
    }
    if (long.class.equals(clazz)) {
      return 0L;
    }
    if (float.class.equals(clazz)) {
      return 0.0f;
    }
    if (double.class.equals(clazz)) {
      return 0d;
    }
    if (char.class.equals(clazz)) {
      return '\u0000';
    }
    if (boolean.class.equals(clazz)) {
      return false;
    }
    throw new SimpleException("无法获取默认值:" + clazz);
  }

  /** Call {@link org.springframework.beans.BeanUtils#copyProperties(Object, Object, String...)} */
  public static void copyProperties(Object source, Object target, String... ignoreProperties)
      throws BeansException {
    org.springframework.beans.BeanUtils.copyProperties(source, target, ignoreProperties);
  }

  /**
   * 获取一个 Type 类型实际对应的Class
   *
   * @param type 类型
   * @return 与Type类型实际对应的Class
   */
  @SuppressWarnings("rawtypes")
  public static Class<?> getTypeClass(Type type) {
    Class<?> clazz = null;
    if (type instanceof Class<?>) {
      clazz = (Class<?>) type;
    } else if (type instanceof ParameterizedType) {
      ParameterizedType pt = (ParameterizedType) type;
      clazz = (Class<?>) pt.getRawType();
    } else if (type instanceof GenericArrayType) {
      GenericArrayType gat = (GenericArrayType) type;
      Class<?> typeClass = getTypeClass(gat.getGenericComponentType());
      return Array.newInstance(typeClass, 0).getClass();
    } else if (type instanceof TypeVariable) {
      TypeVariable tv = (TypeVariable) type;
      Type[] ts = tv.getBounds();
      if (ts != null && ts.length > 0) return getTypeClass(ts[0]);
    } else if (type instanceof WildcardType) {
      WildcardType wt = (WildcardType) type;
      Type[] t_low = wt.getLowerBounds();
      if (t_low.length > 0) return getTypeClass(t_low[0]);
      Type[] t_up = wt.getUpperBounds();
      return getTypeClass(t_up[0]);
    }
    return clazz;
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   * @throws Exception the exception
   */
  public static void main(String[] args) throws Exception {
    BeanUtils.scanClasses("io.github.ramerf", Condition.class)
        .forEach(o -> log.info("main:[{}]", o));

    invoke(null, String.class.getMethods()[0], "string");
    invoke(null, String.class.getMethods()[0], "string")
        .ifPresent(e -> log.info("main:调用失败处理[{}]", e.getClass()));
    log.info("main:[{}]", retrievePrivateFields(Ts.class, ArrayList::new));
    log.info("main:[{}]", getDeclaredField(Ts.class, "name"));
  }
}

/** The type Ts. */
class Ts {
  @TableColumn(updatable = false)
  private long id;

  @TableColumn(name = "alia")
  private String name;

  @TableColumn(insertable = false)
  private String db;

  private transient String idName;
}
