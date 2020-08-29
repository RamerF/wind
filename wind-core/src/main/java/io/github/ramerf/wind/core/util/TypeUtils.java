package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.reflect.ParameterizedTypeImpl;
import java.lang.reflect.Type;

/**
 * The type Type utils.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.21
 */
public class TypeUtils {
  public static Type getType(final Class<?> rawType, final Class<?> actualArgumentType) {
    return new ParameterizedTypeImpl(rawType, new Type[] {actualArgumentType}, null);
  }
}
