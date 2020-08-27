package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * 执行写数据.
 *
 * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
@SuppressWarnings({"UnusedReturnValue"})
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {

  /**
   * 创建记录.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return {@code id}
   * @throws RuntimeException 创建失败时,抛异常
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  default long create(@Nonnull final T t) throws RuntimeException {
    textFilter(t, t);
    getUpdate().create(t);
    return t.getId();
  }

  /**
   * 创建记录.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@code id}
   * @throws RuntimeException 创建失败时,抛异常
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  default long createWithNull(@Nonnull final T t, List<IFunction<T, ?>> includeNullProps)
      throws RuntimeException {
    textFilter(t, t);
    getUpdate().createWithNull(t, includeNullProps);
    return t.getId();
  }

  /**
   * 批量创建.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts the ts
   * @return 受影响的数据库记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatch(final List<T> ts) throws RuntimeException {
    return getUpdate().createBatch(ts);
  }

  /**
   * 批量创建.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return 受影响的数据库记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatchWithNull(
      final List<T> ts, List<IFunction<T, ?>> includeNullProps) throws RuntimeException {
    return getUpdate().createBatchWithNull(ts, includeNullProps);
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @return {@code Optional},只有当更新记录数不等于1时,包含入参为实际受影响的行数
   * @throws RuntimeException the runtime exception
   */
  default Optional<Integer> update(final T t) throws RuntimeException {
    return Optional.of(getUpdate().update(t));
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@code Optional},只有当更新记录数不等于1时,包含入参为实际受影响的行数
   * @throws RuntimeException the runtime exception
   */
  default Optional<Integer> updateWithNull(final T t, List<IFunction<T, ?>> includeNullProps)
      throws RuntimeException {
    return Optional.of(getUpdate().updateWithNull(t, includeNullProps));
  }

  /**
   * 条件更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @param consumer 更新条件
   * @return the 更新记录数
   * @throws RuntimeException the runtime exception
   */
  default int update(@Nonnull final Consumer<ICondition<T>> consumer, final T t)
      throws RuntimeException {
    return getUpdate().where(consumer).update(t);
  }

  /**
   * 条件更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @param consumer 更新条件
   * @param includeNullProps 即使值为null也保存的属性
   * @return the 更新记录数
   * @throws RuntimeException the runtime exception
   */
  default int updateWithNull(
      @Nonnull final Consumer<ICondition<T>> consumer,
      final T t,
      List<IFunction<T, ?>> includeNullProps)
      throws RuntimeException {
    return getUpdate().where(consumer).updateWithNull(t, includeNullProps);
  }

  /**
   * 批量更新.<br>
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts 要更新的数据集
   * @return {@code Optional},只有当{@code ts}不为空且更新记录数和{@code ts}的大小不同时,包含入参为实际受影响的行数
   * @throws DataAccessException 如果更新失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts) throws DataAccessException {
    return getUpdate().updateBatch(ts);
  }

  /**
   * 批量更新.<br>
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts 要更新的数据集
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@code Optional},只有当{@code ts}不为空且更新记录数和{@code ts}的大小不同时,包含入参为实际受影响的行数
   * @throws DataAccessException 如果更新失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatchWithNull(
      final List<T> ts, List<IFunction<T, ?>> includeNullProps) throws DataAccessException {
    return getUpdate().updateBatchWithNull(ts, includeNullProps);
  }

  /**
   * 删除记录.
   *
   * @param id the id
   * @throws RuntimeException 执行失败或者未删除
   * @return 仅当受影响行数不为1时,{@code Optional#isPresent()}为true
   * @see DataAccessException
   * @see CommonException
   */
  default Optional<CommonException> delete(final long id) throws RuntimeException {
    if (getUpdate().where(condition -> condition.eq(AbstractEntityPoJo::setId, id)).delete() != 1) {
      return Optional.of(CommonException.of(ResultCode.API_FAIL_EXEC_DELETE));
    }
    return Optional.empty();
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.<br>
   *     示例:<br>
   *     condition -&gt; condition.eq(AbstractEntityPoJo::setId)<br>
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
   * @return 如果 {@code ids}为空且删除记录数不等于 {@code ids}的大小,返回删除影响行数的 {@code Optional}<br>
   *     否则返回空的 {@code Optional}
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> deleteByIds(final Collection<Long> ids) throws RuntimeException {
    if (CollectionUtils.isEmpty(ids)) {
      return Optional.empty();
    }
    final int affectRow =
        getUpdate().where(condition -> condition.in(AbstractEntityPoJo::setId, ids)).delete();
    return affectRow == ids.size() ? Optional.empty() : Optional.of(affectRow);
  }
}
