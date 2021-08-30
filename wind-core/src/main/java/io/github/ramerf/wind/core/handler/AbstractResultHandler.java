package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Abstract result handler.
 *
 * @since 2020 /4/6
 * @author ramer
 * @param <T> 数据库返回对象
 * @param <E> 实际返回对象
 */
@Slf4j
public abstract class AbstractResultHandler<T, E> implements ResultHandler<T, E> {

  /** The Clazz. */
  final Class<E> clazz;

  /**
   * Instantiates a new Abstract result handler.
   *
   * @param clazz the clazz
   */
  public AbstractResultHandler(@Nonnull final Class<E> clazz) {
    this.clazz = clazz;
  }

  /**
   * {@inheritDoc}
   *
   * @param t the t
   * @return E the clazz
   */
  @Override
  public abstract E handle(T t);

  /**
   * {@inheritDoc}
   *
   * @param ts the ts
   * @return List the list of clazz
   */
  @Override
  public List<E> handle(List<T> ts) {
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
