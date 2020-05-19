package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * 执行写数据,默认不包含值为null的字段.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
@SuppressWarnings({"UnusedReturnValue", "unchecked"})
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param includeNullProps 即使值为null也保存的属性
   * @return the {@link AbstractEntityPoJo}
   * @throws RuntimeException 创建失败时,抛异常
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  default long create(@Nonnull final T t, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    textFilter(t, t);
    getUpdate().create(t, includeNullProps);
    return t.getId();
  }

  /**
   * 批量创建.
   *
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return 受影响的数据库记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  @Transactional(rollbackFor = Exception.class)
  default int createBatch(final List<T> ts, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    return getUpdate().createBatch(ts, includeNullProps);
  }

  /**
   * 更新不为null的字段..
   *
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default int update(final T t, final IFunction<T, ?>... includeNullProps) throws RuntimeException {
    return getUpdate().update(t, includeNullProps);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param consumer 更新条件
   * @param includeNullProps 即使值为null也保存的属性
   * @return the 更新记录数
   * @throws RuntimeException the runtime exception
   */
  default int update(
      @Nonnull final Consumer<ICondition<T>> consumer,
      final T t,
      final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    return getUpdate().where(consumer).update(t, includeNullProps);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@link AbstractEntityPoJo}
   * @throws DataAccessException 如果更新失败
   */
  @Transactional(rollbackFor = Exception.class)
  default int updateBatch(final List<T> ts, final IFunction<T, ?>... includeNullProps)
      throws DataAccessException {
    return getUpdate().updateBatch(ts, includeNullProps);
  }

  /**
   * 删除记录.
   *
   * @param id the id
   * @throws RuntimeException 执行失败或者未删除
   * @see DataAccessException
   * @see CommonException
   */
  default void delete(final long id) throws RuntimeException {
    if (getUpdate().where(condition -> condition.eq(AbstractEntityPoJo::setId, id)).delete() != 1) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_DELETE);
    }
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
}
