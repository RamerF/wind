package io.github.ramerf.wind.core.util;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import javax.annotation.Nullable;

/**
 * Miscellaneous class utility methods. Mainly for internal use within the framework.
 *
 * @author Juergen Hoeller
 * @author Keith Donald
 * @author Rob Harrop
 * @author Sam Brannen
 * @since 1.1
 */
public abstract class ClassUtils {

  /** Suffix for array class names: "[]" */
  public static final String ARRAY_SUFFIX = "[]";

  /** Prefix for internal array class names: "[" */
  private static final String INTERNAL_ARRAY_PREFIX = "[";

  /** Prefix for internal non-primitive array class names: "[L" */
  private static final String NON_PRIMITIVE_ARRAY_PREFIX = "[L";

  /** The package separator character: '.' */
  private static final char PACKAGE_SEPARATOR = '.';

  /** The path separator character: '/' */
  private static final char PATH_SEPARATOR = '/';

  /** The inner class separator character: '$' */
  private static final char INNER_CLASS_SEPARATOR = '$';

  /** The CGLIB class separator: "$$" */
  public static final String CGLIB_CLASS_SEPARATOR = "$$";

  /** The ".class" file suffix */
  public static final String CLASS_FILE_SUFFIX = ".class";

  /**
   * Map with primitive wrapper type as key and corresponding primitive type as value, for example:
   * Integer.class -> int.class.
   */
  private static final Map<Class<?>, Class<?>> primitiveWrapperTypeMap = new IdentityHashMap<>(8);

  /**
   * Map with primitive type name as key and corresponding primitive type as value, for example:
   * "int" -> "int.class".
   */
  private static final Map<String, Class<?>> primitiveTypeNameMap = new HashMap<>(32);

  /**
   * Map with common Java language class name as key and corresponding Class as value. Primarily for
   * efficient deserialization of remote invocations.
   */
  private static final Map<String, Class<?>> commonClassCache = new HashMap<>(64);

  static {
    primitiveWrapperTypeMap.put(Boolean.class, boolean.class);
    primitiveWrapperTypeMap.put(Byte.class, byte.class);
    primitiveWrapperTypeMap.put(Character.class, char.class);
    primitiveWrapperTypeMap.put(Double.class, double.class);
    primitiveWrapperTypeMap.put(Float.class, float.class);
    primitiveWrapperTypeMap.put(Integer.class, int.class);
    primitiveWrapperTypeMap.put(Long.class, long.class);
    primitiveWrapperTypeMap.put(Short.class, short.class);

    // Map entry iteration is less expensive to initialize than forEach with lambdas
    for (Map.Entry<Class<?>, Class<?>> entry : primitiveWrapperTypeMap.entrySet()) {
      registerCommonClasses(entry.getKey());
    }

    Set<Class<?>> primitiveTypes = new HashSet<>(32);
    primitiveTypes.addAll(primitiveWrapperTypeMap.values());
    Collections.addAll(
        primitiveTypes,
        boolean[].class,
        byte[].class,
        char[].class,
        double[].class,
        float[].class,
        int[].class,
        long[].class,
        short[].class);
    primitiveTypes.add(void.class);
    for (Class<?> primitiveType : primitiveTypes) {
      primitiveTypeNameMap.put(primitiveType.getName(), primitiveType);
    }

    registerCommonClasses(
        Boolean[].class,
        Byte[].class,
        Character[].class,
        Double[].class,
        Float[].class,
        Integer[].class,
        Long[].class,
        Short[].class);
    registerCommonClasses(
        Number.class,
        Number[].class,
        String.class,
        String[].class,
        Class.class,
        Class[].class,
        Object.class,
        Object[].class);
    registerCommonClasses(
        Throwable.class,
        Exception.class,
        RuntimeException.class,
        Error.class,
        StackTraceElement.class,
        StackTraceElement[].class);
    registerCommonClasses(
        Enum.class,
        Iterable.class,
        Iterator.class,
        Enumeration.class,
        Collection.class,
        List.class,
        Set.class,
        Map.class,
        Map.Entry.class,
        Optional.class);

    Class<?>[] javaLanguageInterfaceArray = {
      Serializable.class,
      Externalizable.class,
      Closeable.class,
      AutoCloseable.class,
      Cloneable.class,
      Comparable.class
    };
    registerCommonClasses(javaLanguageInterfaceArray);
  }

  /** Register the given common classes with the ClassUtils cache. */
  private static void registerCommonClasses(Class<?>... commonClasses) {
    for (Class<?> clazz : commonClasses) {
      commonClassCache.put(clazz.getName(), clazz);
    }
  }

  /**
   * Return the default ClassLoader to use: typically the thread context ClassLoader, if available;
   * the ClassLoader that loaded the ClassUtils class will be used as fallback.
   *
   * <p>Call this method if you intend to use the thread context ClassLoader in a scenario where you
   * clearly prefer a non-null ClassLoader reference: for example, for class path resource loading
   * (but not necessarily for {@code Class.forName}, which accepts a {@code null} ClassLoader
   * reference as well).
   *
   * @return the default ClassLoader (only {@code null} if even the system ClassLoader isn't
   *     accessible)
   * @see Thread#getContextClassLoader()
   * @see ClassLoader#getSystemClassLoader()
   */
  @Nullable
  public static ClassLoader getDefaultClassLoader() {
    ClassLoader cl = null;
    try {
      cl = Thread.currentThread().getContextClassLoader();
    } catch (Throwable ex) {
      // Cannot access thread context ClassLoader - falling back...
    }
    if (cl == null) {
      // No thread context class loader -> use class loader of this class.
      cl = ClassUtils.class.getClassLoader();
      if (cl == null) {
        // getClassLoader() returning null indicates the bootstrap ClassLoader
        try {
          cl = ClassLoader.getSystemClassLoader();
        } catch (Throwable ex) {
          // Cannot access system ClassLoader - oh well, maybe the caller can live with null...
        }
      }
    }
    return cl;
  }

  /**
   * Replacement for {@code Class.forName()} that also returns Class instances for primitives (e.g.
   * "int") and array class names (e.g. "String[]"). Furthermore, it is also capable of resolving
   * inner class names in Java source style (e.g. "java.lang.Thread.State" instead of
   * "java.lang.Thread$State").
   *
   * @param name the name of the Class
   * @param classLoader the class loader to use (may be {@code null}, which indicates the default
   *     class loader)
   * @return a class instance for the supplied name
   * @throws ClassNotFoundException if the class was not found
   * @throws LinkageError if the class file could not be loaded
   * @see Class#forName(String, boolean, ClassLoader)
   */
  public static Class<?> forName(String name, @Nullable ClassLoader classLoader)
      throws ClassNotFoundException, LinkageError {

    Asserts.notNull(name, "Name must not be null");

    Class<?> clazz = resolvePrimitiveClassName(name);
    if (clazz == null) {
      clazz = commonClassCache.get(name);
    }
    if (clazz != null) {
      return clazz;
    }

    // "java.lang.String[]" style arrays
    if (name.endsWith(ARRAY_SUFFIX)) {
      String elementClassName = name.substring(0, name.length() - ARRAY_SUFFIX.length());
      Class<?> elementClass = forName(elementClassName, classLoader);
      return Array.newInstance(elementClass, 0).getClass();
    }

    // "[Ljava.lang.String;" style arrays
    if (name.startsWith(NON_PRIMITIVE_ARRAY_PREFIX) && name.endsWith(";")) {
      String elementName = name.substring(NON_PRIMITIVE_ARRAY_PREFIX.length(), name.length() - 1);
      Class<?> elementClass = forName(elementName, classLoader);
      return Array.newInstance(elementClass, 0).getClass();
    }

    // "[[I" or "[[Ljava.lang.String;" style arrays
    if (name.startsWith(INTERNAL_ARRAY_PREFIX)) {
      String elementName = name.substring(INTERNAL_ARRAY_PREFIX.length());
      Class<?> elementClass = forName(elementName, classLoader);
      return Array.newInstance(elementClass, 0).getClass();
    }

    ClassLoader clToUse = classLoader;
    if (clToUse == null) {
      clToUse = getDefaultClassLoader();
    }
    try {
      return (clToUse != null ? clToUse.loadClass(name) : Class.forName(name));
    } catch (ClassNotFoundException ex) {
      int lastDotIndex = name.lastIndexOf(PACKAGE_SEPARATOR);
      if (lastDotIndex != -1) {
        String innerClassName =
            name.substring(0, lastDotIndex)
                + INNER_CLASS_SEPARATOR
                + name.substring(lastDotIndex + 1);
        try {
          return (clToUse != null
              ? clToUse.loadClass(innerClassName)
              : Class.forName(innerClassName));
        } catch (ClassNotFoundException ex2) {
          // Swallow - let original exception get through
        }
      }
      throw ex;
    }
  }

  /**
   * Check whether the given class is visible in the given ClassLoader.
   *
   * @param clazz the class to check (typically an interface)
   * @param classLoader the ClassLoader to check against (may be {@code null} in which case this
   *     method will always return {@code true})
   */
  public static boolean isVisible(Class<?> clazz, @Nullable ClassLoader classLoader) {
    if (classLoader == null) {
      return true;
    }
    try {
      if (clazz.getClassLoader() == classLoader) {
        return true;
      }
    } catch (SecurityException ex) {
      // Fall through to loadable check below
    }

    // Visible if same Class can be loaded from given ClassLoader
    return isLoadable(clazz, classLoader);
  }

  /**
   * Check whether the given class is loadable in the given ClassLoader.
   *
   * @param clazz the class to check (typically an interface)
   * @param classLoader the ClassLoader to check against
   * @since 5.0.6
   */
  private static boolean isLoadable(Class<?> clazz, ClassLoader classLoader) {
    try {
      return (clazz == classLoader.loadClass(clazz.getName()));
      // Else: different class with same name found
    } catch (ClassNotFoundException ex) {
      // No corresponding class found at all
      return false;
    }
  }

  /**
   * Resolve the given class name as primitive class, if appropriate, according to the JVM's naming
   * rules for primitive classes.
   *
   * <p>Also supports the JVM's internal class names for primitive arrays. Does <i>not</i> support
   * the "[]" suffix notation for primitive arrays; this is only supported by {@link
   * #forName(String, ClassLoader)}.
   *
   * @param name the name of the potentially primitive class
   * @return the primitive class, or {@code null} if the name does not denote a primitive class or
   *     primitive array class
   */
  @Nullable
  public static Class<?> resolvePrimitiveClassName(@Nullable String name) {
    Class<?> result = null;
    // Most class names will be quite long, considering that they
    // SHOULD sit in a package, so a length check is worthwhile.
    if (name != null && name.length() <= 8) {
      // Could be a primitive - likely.
      result = primitiveTypeNameMap.get(name);
    }
    return result;
  }
}
