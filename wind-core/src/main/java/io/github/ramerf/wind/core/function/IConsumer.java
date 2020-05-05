package io.github.ramerf.wind.core.function;

/**
 * The interface Consumer.
 *
 * @param <T> the type parameter
 * @param <R> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /5/5
 */
@FunctionalInterface
public interface IConsumer<T, R> extends BeanFunction {
  /**
   * Accept.
   *
   * @param t the t
   * @param r the r
   */
  void accept(final T t, final R r);
}
