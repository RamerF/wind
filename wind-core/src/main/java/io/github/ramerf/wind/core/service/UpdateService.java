package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * 执行写数据.
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2020/1/5
 * @see WindConfiguration#isWriteNullProp()
 */
public interface UpdateService<T, ID extends Serializable> extends InterService<T, ID> {

  /**
   * 创建记录.
   *
   * @param e the e
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  default <E> int create(@Nonnull final E e) throws DataAccessException {
    return create(e, null);
  }

  /**
   * 创建记录.
   *
   * @param e the e
   * @param fields the fields
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  default <E> int create(@Nonnull final E e, final Fields<E> fields) throws DataAccessException {
    textFilter(e, e);
    //noinspection unchecked
    return getUpdate((Class<E>) e.getClass()).create(e, fields);
  }

  /**
   * 批量创建.
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
   * @param ts the ts
   * @param fields the fields
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default <E> Optional<Integer> createBatch(@Nonnull final List<E> ts, final Fields<E> fields)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    //noinspection unchecked
    return getUpdate((Class<E>) ts.get(0).getClass()).createBatch(ts, fields);
  }

  /**
   * 更新.
   *
   * @param t the t
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default <E> int update(@Nonnull final E t) throws DataAccessException {
    return update(t, null, null);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default <E> int update(@Nonnull final E object, final Cnd<E, ?, ?> cnd)
      throws DataAccessException {
    return update(object, null, cnd);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @param fields 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default <E> int update(@Nonnull final E object, final Fields<E> fields)
      throws DataAccessException {
    return update(object, fields, null);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @param fields 更新字段
   * @param cnd the cnd
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default <E> int update(@Nonnull final E object, final Fields<E> fields, final Cnd<E, ?, ?> cnd)
      throws DataAccessException {
    if (cnd == null) {
      @SuppressWarnings("unchecked")
      final Class<E> clazz = (Class<E>) object.getClass();
      return getUpdate(clazz).update(object, fields);
    }
    return getUpdate(cnd.getClazz()).where(cnd.getCondition()).update(object, fields);
  }

  /**
   * 批量更新.
   *
   * @param ts 要更新的数据集
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts) throws DataAccessException {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts, final Fields<T> fields)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    return getUpdate().updateBatch(ts, fields);
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
  default int delete(final ID id) throws DataAccessException {
    final LambdaCondition<T> condition = LambdaCondition.of(getPoJoClass());
    return getUpdate()
        .where(condition.eq(EntityHelper.getEntityIdField(getPoJoClass()), id))
        .delete();
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @return 当受影响行数等于 {@code ids.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default Optional<Integer> delete(final Collection<ID> ids) throws DataAccessException {
    if (CollectionUtils.isEmpty(ids)) {
      return Optional.empty();
    }
    final LambdaCondition<T> condition = LambdaCondition.of(getPoJoClass());
    condition.in(EntityHelper.getEntityIdField(getPoJoClass()), ids);
    final int affectRow = getUpdate().where(condition).delete();
    return affectRow == ids.size() ? Optional.empty() : Optional.of(affectRow);
  }

  /**
   * 条件删除.
   *
   * @param condition the condition.
   *     <p>示例:
   *     <li>{@code Cnds.of(Foo.class).eq(Foo::setId, id)}
   *     <li>{@code LambdaCondition.of(Foo.class).eq(Foo::setId, 1L)}
   * @return 删除记录数 long
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  default int delete(@Nonnull Condition<T, ?> condition) throws DataAccessException {
    return getUpdate().where(condition).delete();
  }
}
