package io.github.ramerf.mybatisturbo.core.support;

/**
 * 传入n个string,返回指定类型.
 *
 * @param <R>
 */
@FunctionalInterface
public interface VarArgsFunction<T, R> {
  @SuppressWarnings({"unused", "unchecked"})
  R apply(T... ts);
}
