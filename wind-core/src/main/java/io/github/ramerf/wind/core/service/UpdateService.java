package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
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
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {

  /**
   * 创建记录.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  default long create(@Nonnull final T t) throws DataAccessException {
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
   * @throws DataAccessException 如果执行失败
   */
  default long createWithNull(@Nonnull final T t, List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
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
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatch(final List<T> ts) throws DataAccessException {
    return getUpdate().createBatch(ts);
  }

  /**
   * 批量创建.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatchWithNull(
      final List<T> ts, List<IFunction<T, ?>> includeNullProps) throws DataAccessException {
    return getUpdate().createBatchWithNull(ts, includeNullProps);
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int update(final T t) throws DataAccessException {
    return getUpdate().update(t);
  }

  /**
   * 更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int updateWithNull(final T t, List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
    return getUpdate().updateWithNull(t, includeNullProps);
  }

  /**
   * 条件更新.
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param t the t
   * @param consumer 更新条件
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int update(@Nonnull final Consumer<ICondition<T>> consumer, final T t)
      throws DataAccessException {
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
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int updateWithNull(
      @Nonnull final Consumer<ICondition<T>> consumer,
      final T t,
      List<IFunction<T, ?>> includeNullProps)
      throws DataAccessException {
    return getUpdate().where(consumer).updateWithNull(t, includeNullProps);
  }

  /**
   * 批量更新.<br>
   *
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts 要更新的数据集
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
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
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
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
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   * @see CommonException
   */
  default int delete(final long id) throws DataAccessException {
    return getUpdate().where(condition -> condition.eq(AbstractEntityPoJo::setId, id)).delete();
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.示例:<br>
   *     {@code condition -> condition.eq(AbstractEntityPoJo::setId, 1L)}
   * @return 删除记录数 long
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  default int delete(Consumer<ICondition<T>> consumer) throws DataAccessException {
    return getUpdate().where(consumer).delete();
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @return 当受影响行数等于 {@code ids.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> deleteByIds(final Collection<Long> ids) throws DataAccessException {
    if (CollectionUtils.isEmpty(ids)) {
      return Optional.empty();
    }
    final int affectRow =
        getUpdate().where(condition -> condition.in(AbstractEntityPoJo::setId, ids)).delete();
    return affectRow == ids.size() ? Optional.empty() : Optional.of(affectRow);
  }
}
