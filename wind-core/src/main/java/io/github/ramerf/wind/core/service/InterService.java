package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.io.Serializable;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2020 /1/5
 */
public interface InterService<T, ID extends Serializable> {

  /**
   * 过滤某些属性可能包含的特殊字符.
   *
   * @param trans 原始对象
   * @param filtered 过滤后的对象
   */
  default <E> void textFilter(E trans, E filtered) {}

  /**
   * Gets query.
   *
   * @return the query
   */
  default Query<T> getQuery() {
    return Query.getInstance(getPoJoClass());
  }

  /**
   * Gets query.
   *
   * @return the query
   */
  default <R> Query<R> getQuery(final Class<R> clazz) {
    return Query.getInstance(clazz);
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  default Update<T> getUpdate() {
    return Update.getInstance(getPoJoClass());
  }

  /**
   * Gets update for clazz.
   *
   * @param clazz 是否当前类的更新组件
   * @return the update
   */
  default <R> Update<R> getUpdate(final Class<R> clazz) {
    return Update.getInstance(clazz);
  }

  /** 获取service操作的实体. */
  default Class<T> getPoJoClass() {
    return EntityUtils.getPoJoClass(this);
  }
}
