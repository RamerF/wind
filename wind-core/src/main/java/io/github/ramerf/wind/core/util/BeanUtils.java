package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.condition.QueryEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.*;
import java.beans.FeatureDescriptor;
import java.io.IOException;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.*;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
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
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@SuppressWarnings({"unused"})
public final class BeanUtils {
  /** 对象所有(包含父类)私有字段. */
  private static final Map<Class<?>, WeakReference<List<Field>>> PRIVATE_FIELDS_MAP =
      new ConcurrentHashMap<>();
  /** 对象的写入方法. */
  private static final Map<Class<?>, WeakReference<List<Method>>> WRITE_METHOD_MAP =
      new ConcurrentHashMap<>();
  /** lambda和对应的Field. */
  private static final Map<BeanFunction, WeakReference<Field>> LAMBDA_FIELD_MAP =
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
  public static List<String> getNullProp(@Nonnull final Object obj) {
    final BeanWrapperImpl wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .filter(o -> Objects.isNull(wrapper.getPropertyValue(o.getName())))
        .map(FeatureDescriptor::getName)
        .collect(toList());
  }

  /**
   * 获取对象属性值不为空的属性名.
   *
   * @param obj the obj
   * @return the non null prop
   */
  public static List<String> getNonNullProp(Object obj) {
    final BeanWrapper wrapper = new BeanWrapperImpl(obj);
    return Stream.of(wrapper.getPropertyDescriptors())
        .filter(o -> Objects.nonNull(wrapper.getPropertyValue(o.getName())))
        .map(FeatureDescriptor::getName)
        .filter(o -> !Objects.equals("class", o))
        .collect(toList());
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
   * @param fields the fields
   * @return the list
   */
  public static List<Field> retrievePrivateFields(
      @Nonnull final Class<?> clazz, @Nonnull final List<Field> fields) {
    return Optional.ofNullable(PRIVATE_FIELDS_MAP.get(clazz))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final List<Field> list = getPrivateFields(clazz, fields);
              PRIVATE_FIELDS_MAP.put(clazz, new WeakReference<>(list));
              return list;
            });
  }

  private static List<Field> getPrivateFields(
      @Nonnull Class<?> clazz, @Nonnull final List<Field> fields) {
    do {
      fields.addAll(Arrays.asList(clazz.getDeclaredFields()));
    } while (Objects.nonNull(clazz = clazz.getSuperclass()));
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
              final List<Method> methods =
                  Arrays.stream(clazz.getMethods())
                      .filter(method -> method.getParameterTypes().length > 0)
                      .filter(
                          method -> {
                            final String name = method.getName();
                            return name.startsWith("set") || name.startsWith("is");
                          })
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
    throw CommonException.of(String.format("无法实例化对象[%s]", clazz.getSimpleName()));
  }

  /**
   * 实例化对象.
   *
   * @param <T> the type parameter
   * @param classPath the class path
   * @return the t
   */
  @SuppressWarnings("unchecked")
  public static <T> T initial(final String classPath) {
    try {
      return (T) initial(getClazz(classPath));
    } catch (Exception e) {
      log.warn("initial:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
    throw CommonException.of(String.format("无法实例化对象[%s]", classPath));
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
    throw CommonException.of(String.format("无法获取class[%s]", classPath));
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
      return Optional.ofNullable(consumer).map(ex -> ex.apply(e)).orElse(null);
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
      final Object obj,
      final Field field,
      final Object value,
      Function<Exception, Object> consumer) {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      field.set(obj, value);
    } catch (Exception e) {
      if (consumer != null) {
        consumer.apply(e);
      } else {
        throw CommonException.of(e);
      }
    }
  }

  /**
   * Is primitive type boolean.
   *
   * @param clazz the clazz
   * @return the boolean
   */
  public static boolean isPrimitiveType(final Class<?> clazz) {
    return (ClassUtils.isPrimitiveOrWrapper(clazz)
        || Enum.class.isAssignableFrom(clazz)
        || CharSequence.class.isAssignableFrom(clazz)
        || Number.class.isAssignableFrom(clazz)
        || Date.class.isAssignableFrom(clazz)
        || URI.class == clazz
        || URL.class == clazz
        || Locale.class == clazz
        || Class.class == clazz);
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   * @throws Exception the exception
   */
  public static void main(String[] args) throws Exception {
    BeanUtils.scanClasses("io.github.ramerf", QueryEntity.class)
        .forEach(o -> log.info("main:[{}]", o));

    invoke(null, String.class.getMethods()[0], "string");
    invoke(null, String.class.getMethods()[0], "string")
        .ifPresent(e -> log.info("main:调用失败处理[{}]", e.getClass()));
    log.info("main:[{}]", retrievePrivateFields(Ts.class, new ArrayList<>()));
    log.info("main:[{}]", getDeclaredField(Ts.class, "name"));
  }
}

/** The type Ts. */
@SuppressWarnings("unused")
class Ts extends AbstractEntityPoJo {
  @Column(name = "alia")
  private String name;

  private Integer size;
  private transient String nameSize;
}
