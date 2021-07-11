package io.github.ramerf.wind.core.support;

/**
 * 传入n个变量,返回指定类型.
 *
 * @param <T> the type parameter
 * @param <R> the type parameter
 * @author ramer
 */
@FunctionalInterface
public interface VarArgsFunction<T, R> {
  /**
   * Apply r.
   *
   * @param ts the ts
   * @return the r
   */
  @SuppressWarnings({"unused", "unchecked"})
  R apply(T... ts);
}
