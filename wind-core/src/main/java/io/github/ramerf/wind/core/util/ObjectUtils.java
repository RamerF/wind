package io.github.ramerf.wind.core.util;

/**
 * The type Object utils.
 *
 * @author ramer
 * @since 2020/4/6
 */
@SuppressWarnings("unused")
public class ObjectUtils {
  /**
   * 如果给定的值不为{@code null} ,执行 {@code runnable}.
   *
   * @param obj the obj
   * @param exec the runnable
   */
  public static void execIfAbsent(final Object obj, final Runnable exec) {
    if (obj != null) {
      exec.run();
    }
  }

  /**
   * 如果给定的值{@code obj}不为空,返回absent.
   *
   * @param <T> the type parameter
   * @param obj 给定值
   * @param absent the absent
   * @return the t
   */
  public static <T> T absentIfNonnull(final T obj, final T absent) {
    return obj == null ? null : absent;
  }
}
