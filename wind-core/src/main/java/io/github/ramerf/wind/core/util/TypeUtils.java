package io.github.ramerf.wind.core.util;

import java.lang.reflect.Type;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

/**
 * The type Type utils.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.21
 */
public class TypeUtils {
  public static Type getType(final Class<?> rawType, final Class<?> actualArgumentType) {
    return ParameterizedTypeImpl.make(rawType, new Type[] {actualArgumentType}, null);
  }
}
