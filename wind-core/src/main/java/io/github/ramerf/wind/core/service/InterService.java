package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.io.Serializable;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
public interface InterService<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable> {

  /**
   * 过滤某些属性可能包含的特殊字符.
   *
   * @param trans 原始对象
   * @param filtered 过滤后的对象
   */
  default void textFilter(T trans, T filtered) {}

  /**
   * Gets query column.
   *
   * @return the query column
   */
  default QueryColumn<T> getQueryColumn() {
    return QueryColumnFactory.fromClass(getPoJoClass());
  }

  /**
   * Gets query.
   *
   * @return the query
   */
  default Query<T> getQuery() {
    return Query.getInstance(getPoJoClass());
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
  default <R extends AbstractEntityPoJo<R, ?>> Update<R> getUpdate(final Class<R> clazz) {
    return Update.getInstance(clazz);
  }

  /**
   * 获取service泛型PoJo.
   *
   * @return the po jo class
   */
  default Class<T> getPoJoClass() {
    return EntityUtils.getPoJoClass(this);
  }

  /**
   * 该方法作为扩展,可在service接口中获取到repository.
   *
   * @param <U> the type parameter
   * @return the repository
   * @throws RuntimeException the runtime exception
   */
  default <U> U getRepository() throws RuntimeException {
    throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
  }
}
