package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;

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
  default List<E> handle(List<T> ts) {
    if (CollectionUtils.isEmpty(ts)) {
      return Collections.emptyList();
    }
    List<E> es = new ArrayList<>();
    for (T t : ts) {
      es.add(handle(t));
    }
    return es;
  }
}
