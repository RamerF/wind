package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.asm.ClassMetadata;
import io.github.ramerf.wind.core.asm.ClassReader;
import io.github.ramerf.wind.core.asm.tree.ClassNode;
import io.github.ramerf.wind.core.exception.*;
import io.github.ramerf.wind.core.io.Resource;
import io.github.ramerf.wind.core.io.ResourceResolver;
import java.io.IOException;
import java.lang.annotation.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Bean utils.
 *
 * @since 2022.02.09
 * @author ramer
 */
@Slf4j
public final class BeanUtils {
  private static final Map<Class<?>, WeakReference<Collection<Field>>> PRIVATE_FIELDS_MAP =
      new ConcurrentHashMap<>();

  private static final Set<String> IGNORED_FILES = new HashSet<>();

  static {
    IGNORED_FILES.add("io\\github\\ramerf\\wind\\core\\io\\VfsUtils.class]");
    IGNORED_FILES.add("io\\github\\ramerf\\wind\\core\\io\\VfsPatternUtils.class]");
  }

  public static ArrayList<Field> retrieveDeclaredFields(@Nonnull final Class<?> clazz) {
    ArrayList<Field> container = new ArrayList<>();
    return retrieveDeclaredFields(clazz, container);
  }

  public static <T extends Collection<Field>> T retrieveDeclaredFields(
      @Nonnull final Class<?> clazz, @Nonnull final T container) {
    //noinspection unchecked
    return Optional.ofNullable(PRIVATE_FIELDS_MAP.get(clazz))
        .map(Reference::get)
        .map(t -> (T) t)
        .orElseGet(
            () -> {
              final T collection = getDeclaredFields(clazz, container);
              PRIVATE_FIELDS_MAP.put(clazz, new WeakReference<>(collection));
              return collection;
            });
  }

  private static <T extends Collection<Field>> T getDeclaredFields(
      @Nonnull Class<?> clazz, @Nonnull final T containers) {
    do {
      for (final Field superField : clazz.getDeclaredFields()) {
        // 同名覆盖只保留子类字段
        if (containers.stream().noneMatch(o -> o.getName().equals(superField.getName()))) {
          containers.add(superField);
        }
      }
    } while ((clazz = clazz.getSuperclass()) != null);
    return containers;
  }

  public static <T> T initial(final Class<T> clazz) throws ClassInstantiationException {
    try {
      return clazz.newInstance();
    } catch (InstantiationException | IllegalAccessException e) {
      throw new ClassInstantiationException(
          String.format("Cannot get instance for class[%s]", clazz.getSimpleName()), e);
    }
  }

  public static <T> T initial(final String classPath) throws ClassInstantiationException {
    return initial(getClazz(classPath));
  }

  @SuppressWarnings("all")
  public static <T> Class<T> getClazz(String classPath) throws ClassInstantiationException {
    if (classPath.contains("/")) {
      classPath = classPath.replaceAll("/", ".");
    }
    try {
      return (Class<T>) Class.forName(classPath);
    } catch (ClassNotFoundException e) {
      throw new ClassInstantiationException(String.format("Cannot load class[%s]", classPath), e);
    }
  }

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
    } while ((clazz = clazz.getSuperclass()) != null && !Object.class.equals(clazz));
    return null;
  }

  /**
   * 扫描指定路径下对象的子类,不包含当前对象.
   *
   * @param packagePatterns 扫描路径,支持路径分割符 <code>,; \t\n</code>
   * @param assignableType 当前对象
   */
  public static <T> Set<Class<? extends T>> scanClasses(
      String packagePatterns, Class<T> assignableType) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
    String[] packagePatternArray = StringUtils.tokenizeToStringArray(packagePatterns, ",; \t\n");
    for (String packagePattern : packagePatternArray) {
      final Resource[] resources =
          ResourceResolver.getResources(
              "classpath*:" //
                  .concat(ResourceResolver.convertToResourcePath(packagePattern))
                  .concat("/**/*.class"));
      for (Resource resource : resources) {
        if (IGNORED_FILES.stream().anyMatch(o -> resource.toString().endsWith(o))) {
          continue;
        }
        try {
          ClassReader classReader = new ClassReader(resource.getInputStream());
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
          log.warn("scanClasses:Cannot load the[resource:{},CausedBy:{}]", resource, e.toString());
        }
      }
    }
    return classes;
  }

  public static <T> Set<Class<? extends T>> scanClassesWithAnnotation(
      String packagePatterns, Class<? extends Annotation> annotation) throws IOException {
    Set<Class<? extends T>> classes = new HashSet<>();
    String[] packagePatternArray = StringUtils.tokenizeToStringArray(packagePatterns, ",; \t\n");
    for (String packagePattern : packagePatternArray) {
      final Resource[] resources =
          ResourceResolver.getResources(
              "classpath*:"
                  .concat(ResourceResolver.convertToResourcePath(packagePattern))
                  .concat("/**/*.class"));
      for (Resource resource : resources) {
        if (IGNORED_FILES.stream().anyMatch(o -> resource.toString().endsWith(o))) {
          continue;
        }
        try {
          ClassReader classReader = new ClassReader(resource.getInputStream());
          ClassNode classNode = new ClassNode();
          classReader.accept(classNode, ClassReader.SKIP_DEBUG);
          final ClassMetadata classMetadata = new ClassMetadata(classNode);
          Class<?> clazz = classMetadata.getCurrentClass();
          if (clazz.isAnnotationPresent(annotation)) {
            //noinspection unchecked
            classes.add((Class<? extends T>) clazz);
          }
        } catch (Throwable e) {
          log.warn("Cannot load the[resource:{},CausedBy:{}]", resource, e.toString());
        }
      }
    }
    return classes;
  }

  public static Method getDeclaredMethod(
      @Nonnull Class<?> clazz, final String methodName, final Class<?>... parameterTypes) {
    do {
      try {
        return clazz.getDeclaredMethod(methodName, parameterTypes);
      } catch (NoSuchMethodException | SecurityException ignored) {
      }
    } while ((clazz = clazz.getSuperclass()) != null && !Object.class.equals(clazz));
    return null;
  }

  public static Set<Method> retrieveMethods(@Nonnull Class<?> clazz) {
    Set<Method> methods = new HashSet<>();
    do {
      methods.addAll(Arrays.asList(clazz.getDeclaredMethods()));
    } while ((clazz = clazz.getSuperclass()) != null && !clazz.equals(Object.class));
    return methods;
  }

  public static Set<Method> scanMethodsWithAnnotation(
      final Class<?> clazz, final Class<? extends Annotation> annotationClass) throws IOException {
    return retrieveMethods(clazz).stream()
        .filter(o -> o.getAnnotation(annotationClass) != null)
        .collect(Collectors.toSet());
  }

  /**
   * 调用指定方法.
   *
   * @throws ReflectiveInvokeException 包装实际异常
   * @see #invokeMethodIgnoreException(Object, Method, Object...)
   */
  public static Object invokeMethod(final Object obj, @Nonnull Method method, Object... value)
      throws ReflectiveInvokeException {
    try {
      if (!method.isAccessible()) {
        method.setAccessible(true);
      }
      return method.invoke(obj, value);
    } catch (IllegalAccessException
        | IllegalArgumentException
        | InvocationTargetException
        | NullPointerException e) {
      throw new ReflectiveInvokeException(e);
    }
  }

  /** 调用指定方法,忽略错误. */
  public static Object invokeMethodIgnoreException(
      final Object obj, Method method, Object... value) {
    try {
      return invokeMethod(obj, method, value);
    } catch (ReflectiveInvokeException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return null;
    }
  }

  /**
   * 获取字段的值.
   *
   * @throws ReflectiveInvokeException 包装实际异常
   * @see #getFieldValueIgnoreException(Object, Field)
   */
  public static Object getFieldValue(final Object obj, final Field field)
      throws ReflectiveInvokeException {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      return field.get(obj);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      throw new ReflectiveInvokeException(e);
    }
  }

  /** 获取字段的值,忽略错误. */
  public static Object getFieldValueIgnoreException(final Object obj, final Field field) {
    try {
      return getFieldValue(obj, field);
    } catch (ReflectiveInvokeException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return null;
    }
  }

  /**
   * 设置字段的值.
   *
   * @throws ReflectiveInvokeException 包装实际异常
   * @see #setFieldValueIgnoreException(Object, Field, Object)
   */
  public static void setFieldValue(final Object obj, final Field field, final Object value)
      throws ReflectiveInvokeException {
    try {
      if (!field.isAccessible()) {
        field.setAccessible(true);
      }
      field.set(obj, value);
    } catch (IllegalArgumentException | IllegalAccessException e) {
      throw new ReflectiveInvokeException(e);
    }
  }

  /** 设置字段的值,忽略错误. */
  public static void setFieldValueIgnoreException(
      final Object obj, final Field field, final Object value) {
    try {
      setFieldValue(obj, field, value);
    } catch (ReflectiveInvokeException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
  }

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
    throw new WindException("无法获取默认值:" + clazz);
  }

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

  /** 获取所有接口. */
  public static Set<Class<?>> getAllInterfaces(Class<?> clazz) {
    Set<Class<?>> allInterfaces = new HashSet<>();
    Deque<Class<?>> deque = new ArrayDeque<>();
    deque.push(clazz);
    while (!deque.isEmpty()) {
      final Class<?> currentClazz = deque.pop();
      final Class<?>[] interfaces = currentClazz.getInterfaces();
      for (Class<?> aClass : interfaces) {
        deque.add(aClass);
        allInterfaces.add(aClass);
      }
    }
    return allInterfaces;
  }
}
