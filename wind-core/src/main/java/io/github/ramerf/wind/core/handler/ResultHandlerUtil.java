package io.github.ramerf.wind.core.handler;

import javax.annotation.Nullable;

/**
 * @author ramer
 * @since 30/08/2021
 */
public final class ResultHandlerUtil {
  public static <E, R> ResultHandler<R> getResultHandler(final Class<R> clazz) {
    return getResultHandler(clazz, null);
  }

  public static <E, R> ResultHandler<R> getResultHandler(
      final Class<R> clazz, @Nullable final ResultHandler<R> defaultResultHandler) {
    if (defaultResultHandler != null) {
      return defaultResultHandler;
    }
    return clazz.getClassLoader() == null || clazz.isArray()
        ? new PrimitiveResultHandler<>(clazz)
        : new BeanResultHandler<>(clazz);
  }
}
