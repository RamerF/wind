package io.github.ramerf.wind.core.handler;

import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;

/**
 * @author ramer
 * @since 30/08/2021
 */
public final class ResultHandlerUtil {

  public static <E, R> R handle(final Map<String, Object> result, final Class<R> clazz) {
    return handle(result, clazz, null);
  }

  public static <E, R> R handle(
      final Map<String, Object> result,
      final Class<R> clazz,
      @Nullable final ResultHandler<Map<String, Object>, R> defaultResultHandler) {
    if (defaultResultHandler != null) {
      return defaultResultHandler.handle(result);
    }
    final ResultHandler<Map<String, Object>, R> resultHandler =
        clazz.getClassLoader() == null || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz)
            : new BeanResultHandler<>(clazz);
    return resultHandler.handle(result);
  }

  public static <E, R> List<R> handle(
      final List<Map<String, Object>> result, final Class<R> clazz) {
    return handle(result, clazz, null);
  }

  public static <E, R> List<R> handle(
      final List<Map<String, Object>> result,
      final Class<R> clazz,
      @Nullable final ResultHandler<Map<String, Object>, R> defaultResultHandler) {
    if (defaultResultHandler != null) {
      return defaultResultHandler.handle(result);
    }
    return clazz.getClassLoader() == null || clazz.isArray()
        ? new PrimitiveResultHandler<>(clazz).handle(result)
        : new BeanResultHandler<>(clazz).handle(result);
  }
}
