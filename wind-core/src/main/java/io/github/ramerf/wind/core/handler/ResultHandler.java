package io.github.ramerf.wind.core.handler;

import java.util.List;

/**
 * 返回结果转换.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author ramer
 * @since 2019 /12/27
 * @see AbstractResultHandler
 */
public interface ResultHandler<T, E> {

  /**
   * Handler e.
   *
   * @param t the t
   * @return the e
   */
  E handle(T t);

  /**
   * Handler list.
   *
   * @param ts the ts
   * @return the list
   */
  List<E> handle(List<T> ts);
}
