package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * The interface Update service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(UpdateService.class);

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return the {@link AbstractEntityPoJo}
   * @throws RuntimeException 创建失败时,抛异常
   * @see DataAccessException
   */
  default long create(T t) throws RuntimeException {
    textFilter(t, t);
    final int row = getUpdate().create(t);
    if (row != 1) {
      throw CommonException.of(ResultCode.ERROR);
    }
    return t.getId();
  }

  /**
   * Create batch list.
   *
   * @param ts the ts
   * @return the list
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default int createBatch(List<T> ts) throws RuntimeException {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    return CollectionUtils.isEmpty(ts) ? 0 : getUpdate().createBatch(ts);
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
   * @param <R> the type parameter
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
   * @return 删除记录数 int
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default int deleteBatch(final Collection<Long> ids) throws RuntimeException {
    if (CollectionUtils.isEmpty(ids)) {
      return 0;
    }
    return getUpdate().where(condition -> condition.in(AbstractEntityPoJo::setId, ids)).delete();
  }

  /**
   * 条件更新,<b>不更新值为<code>null</code>的列</b>.
   *
   * @param t the t
   * @param consumer the consumer
   * @return 更新记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long updateBatch(@Nonnull final T t, @Nonnull final Consumer<ICondition<T>> consumer)
      throws RuntimeException {
    //    return getUpdate(true).where(consumer).update(t, false);
    return 0;
  }

  /**
   * 条件更新,<b>更新所有列(即使值为<code>null</code>的)</b>.
   *
   * @param t the t
   * @param consumer the consumer
   * @return 更新记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long updateBatchAll(@Nonnull final T t, @Nonnull final Consumer<ICondition<T>> consumer)
      throws RuntimeException {
    //    return getUpdate(true).where(consumer).update(t, true);
    return 0;
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.<br>
   *     示例:
   *     <pre>
   *      condition -&gt; condition.eq(AbstractEntityPoJo::setId)
   *     </pre>
   *
   * @return 删除记录数 long
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long delete(Consumer<ICondition<T>> consumer) throws RuntimeException {
    return getUpdate().where(consumer).delete();
  }
}
