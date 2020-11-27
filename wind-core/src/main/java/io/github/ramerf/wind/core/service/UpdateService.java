package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.LambdaCondition;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * 执行写数据.
 *
 * <p>Note:默认不写入值为null的属性,通过{@link WindConfiguration#isWriteNullProp()}开关
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020/1/5
 * @see WindConfiguration#isWriteNullProp()
 */
public interface UpdateService<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
    extends InterService<T, ID> {

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  default T create(@Nonnull final T t) throws DataAccessException {
    return create(t, null);
  }

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param fieldsConsumer the fields consumer
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  default T create(@Nonnull final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    textFilter(t, t);
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) t.getClass());
      fieldsConsumer.accept(fields);
    }
    return getUpdate().create(t, fields);
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
   * @param fieldsConsumer the fields consumer
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings({"unchecked", "DuplicatedCode"})
  default Optional<Integer> createBatch(
      @Nonnull final List<T> ts, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) ts.get(0).getClass());
      fieldsConsumer.accept(fields);
    }
    return getUpdate().createBatch(ts, fields);
  }

  /**
   * 更新.
   *
   * @param t the t
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int update(final T t) throws DataAccessException {
    return update(t, null, null);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param fieldsConsumer 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int update(final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    return update(t, fieldsConsumer, null);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param consumer 更新条件
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int updateByCondition(final T t, final Consumer<LambdaCondition<T>> consumer)
      throws DataAccessException {
    return update(t, null, consumer);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param conditionConsumer 更新条件
   * @param fieldsConsumer 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  default int update(
      final T t,
      Consumer<Fields<T>> fieldsConsumer,
      final Consumer<LambdaCondition<T>> conditionConsumer)
      throws DataAccessException {
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) t.getClass());
      fieldsConsumer.accept(fields);
    }
    if (conditionConsumer == null) {
      return getUpdate().update(t, fields);
    }
    final LambdaCondition<T> condition =
        LambdaCondition.getInstance(QueryColumn.fromClass(getPoJoClass()));
    conditionConsumer.accept(condition);
    return getUpdate().where(condition).update(t, fields);
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
  default Optional<Integer> updateBatch(final List<T> ts) throws DataAccessException {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @param fieldsConsumer the fields consumer
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings({"unchecked", "DuplicatedCode"})
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) ts.get(0).getClass());
      fieldsConsumer.accept(fields);
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
    final LambdaCondition<T> condition =
        LambdaCondition.getInstance(QueryColumn.fromClass(getPoJoClass()));
    return getUpdate()
        .where(condition.eq(EntityHelper.getEntityIdField(getPoJoClass()), id))
        .delete();
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
  default int delete(@Nonnull Consumer<LambdaCondition<T>> consumer) throws DataAccessException {
    final LambdaCondition<T> condition =
        LambdaCondition.getInstance(QueryColumn.fromClass(getPoJoClass()));
    consumer.accept(condition);
    return getUpdate().where(condition).delete();
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @return 当受影响行数等于 {@code ids.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default Optional<Integer> deleteByIds(final Collection<ID> ids) throws DataAccessException {
    if (CollectionUtils.isEmpty(ids)) {
      return Optional.empty();
    }
    final LambdaCondition<T> condition =
        LambdaCondition.getInstance(QueryColumn.fromClass(getPoJoClass()));
    condition.in(EntityHelper.getEntityIdField(getPoJoClass()), ids);
    final int affectRow = getUpdate().where(condition).delete();
    return affectRow == ids.size() ? Optional.empty() : Optional.of(affectRow);
  }
}
