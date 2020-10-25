package io.github.ramerf.wind.core.function;

import java.util.Comparator;

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
  Comparator<IConsumer> COMPARATOR = (o1, o2) -> o1.getField().equals(o2.getField()) ? 0 : 1;
  /**
   * Accept.
   *
   * @param t the t
   * @param u the u
   */
  void accept(final T t, final U u);
}
