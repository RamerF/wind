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
@SuppressWarnings({"UnusedReturnValue", "unchecked"})
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {

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
  default long create(@Nonnull final T t, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    textFilter(t, t);
    getUpdate().create(t, includeNullProps);
    return t.getId();
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
  default int createBatch(final List<T> ts, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    return getUpdate().createBatch(ts, includeNullProps);
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
  default Optional<Integer> update(final T t, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    final int affectRow = getUpdate().update(t, includeNullProps);
    return affectRow == 1 ? Optional.empty() : Optional.of(affectRow);
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
   * <h2><font color="yellow">默认不包含值为null的属性.</font></h2>
   *
   * @param ts 要更新的数据集
   * @param includeNullProps 即使值为null也保存的属性
   * @return {@code Optional},只有当{@code ts}不为空且删除记录数和{@code ts}的大小不同时,包含入参为实际受影响的行数
   * @throws DataAccessException 如果更新失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts, final IFunction<T, ?>... includeNullProps)
      throws DataAccessException {
    return getUpdate().updateBatch(ts, includeNullProps);
  }

  /**
   * 删除记录.
   *
   * @param id the id
   * @throws RuntimeException 执行失败或者未删除
   * @return an {@code Optional} with the {@code CommonException}
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
