package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.github.ramerf.wind.core.util.EntityUtils.getPoJoClass;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public interface InterService<T extends AbstractEntityPoJo> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(InterService.class);

  /**
   * 过滤某些属性可能包含的特殊字符.
   *
   * @param trans 页面传递过来的对象
   * @param filtered 过滤后的对象
   */
  default void textFilter(T trans, T filtered) {}

  /**
   * Gets query column.
   *
   * @return the query column
   */
  default QueryColumn<T> getQueryColumn() {
    return QueryColumnFactory.getInstance(getPoJoClass(this));
  }

  /**
   * Gets query.
   *
   * @return the query
   */
  default Query getQuery() {
    return Query.getInstance();
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  default Update getUpdate() {
    return getUpdate(true);
  }

  /**
   * Gets update for current.
   *
   * @param current 是否当前类的更新组件
   * @return the update
   */
  default Update getUpdate(final boolean current) {
    final Update instance = Update.getInstance();
    return current ? instance.from(getPoJoClass(this)) : instance;
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
