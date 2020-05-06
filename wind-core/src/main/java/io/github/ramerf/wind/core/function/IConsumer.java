package io.github.ramerf.wind.core.function;

/**
 * The interface Consumer.
 *
 * @param <T> the type parameter
 * @param <U> the type parameter
 * @author Tang Xiaofeng
 * @since 2020/5/5
 */
@FunctionalInterface
public interface IConsumer<T, U> extends BeanFunction {
  /**
   * Accept.
   *
   * @param t the t
   * @param u the u
   */
  void accept(final T t, final U u);
}
