package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.asm.ClassMetadata;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.exception.NotImplementedException;
import io.github.ramerf.wind.core.function.FieldFunction;
import java.io.*;
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
import jdk.internal.org.objectweb.asm.ClassReader;
import jdk.internal.org.objectweb.asm.tree.ClassNode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;

import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;

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

  /** Map转Bean. */
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
              } catch (IllegalAccessException
                  | IllegalArgumentException
                  | InvocationTargetException e) {
                log.info(
                    "mapToBean:跳过类型不匹配的字段[{} {}->{}]",
                    methodToProperty(f.getName()),
                    f.getParameterTypes()[0].getSimpleName(),
                    value.getClass().getSimpleName());
              }
            });
    return r;
  }

  /** 获取所有(包含父类)private属性. */
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

  /** 实例化对象. */
  public static <T> T initial(final Class<T> clazz) throws CommonException {
    try {
      return clazz.newInstance();
    } catch (InstantiationException | IllegalAccessException e) {
      throw new CommonException(
          String.format("Cannot get instance for class[%s]", clazz.getSimpleName()), e);
    }
  }

  /** 实例化对象. */
  public static <T> T initial(final String classPath) throws CommonException {
    try {
      return initial(getClazz(classPath));
    } catch (CommonException e) {
      throw new CommonException(String.format("Cannot initial class [%s]", classPath), e);
    }
  }

  /** 通过类路径获取Class. */
  @SuppressWarnings("all")
  public static <T> Class<T> getClazz(String classPath) throws CommonException {
    if (classPath.contains("/")) {
      classPath = classPath.replaceAll("/", ".");
    }
    try {
      return (Class<T>) Class.forName(classPath);
    } catch (ClassNotFoundException e) {
      throw new CommonException(String.format("Cannot load class[%s]", classPath), e);
    }
  }

  /** 通过方法名获取bean属性名. */
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

  public static Field getDeclaredField(@Nonnull Class<?> clazz, final String name) {
    do {
      try {
        return clazz.getDeclaredField(name);
      } catch (NoSuchFieldException | SecurityException ignored) {
      }
    } while ((clazz = clazz.getSuperclass()) != null && Object.class.equals(clazz));
    return null;
  }

  /**
   * 获取指定包下,指定接口/类的子类,不包含自身.
   *
   * @param <T> the type parameter
   * @param packagePatterns 支持多个包名分隔符: ",; \t\n"
   * @param assignableType 父接口/类
   * @return 所有子类,不包含自身assignableType
   * @throws IOException the IOException
   */
  public static <T> Set<Class<? extends T>> scanClasses(
      String packagePatterns, Class<T> assignableType) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
    String[] packagePatternArray = StringUtils.tokenizeToStringArray(packagePatterns, ",; \t\n");
    for (String packagePattern : packagePatternArray) {
      final Set<File> files =
          ResourceUtils.getFiles(
              "classpath*:" //
                  .concat(StringUtils.convertToResourcePath(packagePattern))
                  .concat("/**/*.class"));
      for (File file : files) {
        try {
          ClassReader classReader = new ClassReader(new FileInputStream(file));
          ClassNode classNode = new ClassNode();
          classReader.accept(classNode, ClassReader.SKIP_DEBUG);
          final ClassMetadata classMetadata = new ClassMetadata(classNode);
          Class<?> clazz = classMetadata.getCurrentClass();
          if (!clazz.equals(assignableType)
              && (assignableType == null || assignableType.isAssignableFrom(clazz))) {
            //noinspection unchecked
            classes.add((Class<? extends T>) clazz);
          }
        } catch (Throwable e) {
          log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", file, e.toString());
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
      String packagePatterns, Class<? extends Annotation> annotation) throws IOException
        //   Set<Class<? extends T>> classes = new HashSet<>();
        //   String[] packagePatternArray = tokenizeToStringArray(packagePatterns, ",; \t\n");
        //   for (String packagePattern : packagePatternArray) {
        //     Resource[] resources =
        //         new PathMatchingResourcePatternResolver()
        //             .getResources(
        //                 ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX
        //                     .concat(ClassUtils.convertClassNameToResourcePath(packagePattern))
        //                     .concat("/**/*.class"));
        //     for (Resource resource : resources) {
        //       try {
        //         ClassMetadata classMetadata =
        //             new
        // CachingMetadataReaderFactory().getMetadataReader(resource).getClassMetadata();
        //         Class<T> clazz = BeanUtils.getClazz(classMetadata.getClassName());
        //         if (annotation == null || clazz.isAnnotationPresent(annotation)) {
        //           classes.add(clazz);
        //         }
        //       } catch (Throwable e) {
        //         log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", resource,
        // e.toString());
        //       }
        //     }
        //   }
        //   return classes;
      {
    throw new NotImplementedException("scanClassesWithAnnotation");
  }

  /** 获取类所有方法,包括父类 */
  public static Set<Method> retrieveMethods(@Nonnull Class<?> clazz) {
    Set<Method> methods = new HashSet<>();
    do {
      methods.addAll(Arrays.asList(clazz.getDeclaredMethods()));
    } while ((clazz = clazz.getSuperclass()) != null && !clazz.equals(Object.class));
    return methods;
  }

  /** 获取指定类中包含指定注解的方法 */
  public static Set<Method> scanMethodsWithAnnotation(
      final Class<?> clazz, final Class<? extends Annotation> annotationClass) throws IOException {
    return retrieveMethods(clazz).stream()
        .filter(o -> o.getAnnotation(annotationClass) != null)
        .collect(Collectors.toSet());
  }

  /**
   * {@link Method#invoke(Object, Object...)}<br>
   * 用法:
   *
   * <pre>
   *  BeanUtils.invoke(null, String.class.getMethods()[0], "string")
   *         .ifPresent(e -&gt; log.info(" BeanUtils.main:调用失败处理[{}]", e.getClass()));
   * </pre>
   */
  public static Optional<Exception> invoke(Object obj, Method method, Object... value) {
    try {
      if (!method.isAccessible()) {
        method.setAccessible(true);
      }
      method.invoke(obj, value);
      return Optional.empty();
    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
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
   * @param consumer 异常时的处理,默认抛出异常
   */
  public static Object getValue(
      final Object obj, final Field field, final Function<RuntimeException, Object> consumer) {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      return field.get(obj);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      return Optional.ofNullable(consumer)
          .map(
              ex ->
                  ex.apply(
                      e instanceof IllegalAccessException
                          ? new CommonException(e)
                          : (IllegalArgumentException) e))
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
   * @param consumer 异常时的处理,null时抛出异常
   */
  public static void setValue(
      final Object obj, final Field field, final Object value, Consumer<RuntimeException> consumer)
      throws CommonException {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      field.set(obj, value);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      if (consumer != null) {
        consumer.accept(
            e instanceof IllegalAccessException
                ? new CommonException(e)
                : (IllegalArgumentException) e);
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
    throw new CommonException("无法获取默认值:" + clazz);
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
}
