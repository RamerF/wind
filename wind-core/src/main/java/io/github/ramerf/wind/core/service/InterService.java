package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;
import java.util.function.Consumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;

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
   * Gets by id.
   *
   * @param id the id
   * @return the by id
   */
  default T getById(final long id) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    final Condition<T> condition = queryColumn.getCondition().eq(AbstractEntityPoJo::setId, id);
    return getQuery().select(queryColumn).where(condition).fetchOne(getPoJoClass(this));
  }

  /**
   * List by ids list.
   *
   * @param ids the ids
   * @return the list
   */
  default List<T> listByIds(final Collection<Long> ids) {
    if (CollectionUtils.isEmpty(ids)) {
      return Collections.emptyList();
    }
    final QueryColumn<T> queryColumn = getQueryColumn();
    final Condition<T> condition = queryColumn.getCondition().in(AbstractEntityPoJo::setId, ids);
    return getQuery().select(queryColumn).where(condition).fetchAll(getPoJoClass(this));
  }

  /**
   * 更新不为null的字段..
   *
   * @param t the t
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default int update(T t) throws RuntimeException {
    return getUpdate().update(t, false);
  }

  /**
   * 注意: 该方法只会更新不为null的字段.
   *
   * @param t the t
   * @param consumer 更新条件
   * @return the 更新记录数
   * @throws RuntimeException the runtime exception
   */
  default int update(T t, Consumer<ICondition<T>> consumer) throws RuntimeException {
    return getUpdate().where(consumer).update(t, false);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @param includeNullProperties the include null properties
   * @return {@link AbstractEntityPoJo}
   * @throws RuntimeException sqlException
   */
  @Transactional(rollbackFor = Exception.class)
  default int updateBatch(final List<T> ts, final String... includeNullProperties)
      throws RuntimeException {
    return getUpdate().updateBatch(ts, false);
  }

  /**
   * 批量更新.<br>
   * 注意: 该方法会将ts中的值完全覆盖对应的数据.<br>
   *
   * @param ts 要更新的数据集
   * @param includeNullProperties the include null properties
   * @return {@link AbstractEntityPoJo}
   * @throws RuntimeException sqlException
   */
  @Transactional(rollbackFor = Exception.class)
  default <R extends AbstractEntityRequest<?>> int updateBatchRequest(
      List<R> ts, final String... includeNullProperties) throws RuntimeException {
    return getUpdate().updateBatchRequest(ts, false);
  }

  /**
   * Delete.
   *
   * @param id the id
   * @throws RuntimeException the runtime exception
   */
  default void delete(final long id) throws RuntimeException {
    getUpdate().where(condition -> condition.eq(AbstractEntityPoJo::setId, id)).delete();
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @throws RuntimeException the runtime exception
   * @return 删除记录数
   */
  @Transactional(rollbackFor = Exception.class)
  default int deleteBatch(final Collection<Long> ids) throws RuntimeException {
    if (CollectionUtils.isEmpty(ids)) {
      return 0;
    }
    return getUpdate().where(condition -> condition.in(AbstractEntityPoJo::setId, ids)).delete();
  }

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
   * Gets repository.
   *
   * @param <U> the type parameter
   * @return the repository
   * @throws RuntimeException the runtime exception
   */
  <U> U getRepository() throws RuntimeException;
}
