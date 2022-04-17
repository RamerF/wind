package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.*;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * 通用业务方法.
 *
 * @param <T> 实体
 * @param <ID> 主键类型
 * @author ramer
 * @since 2019/11/13
 */
public interface BaseService<T, ID extends Serializable> {
  /* 读取  */

  default long count(@Nullable final Condition<T, ?> condition) {
    if (condition == null) {
      Class<T> clazz = getPoJoClass();
      return this.getDao().fetchCount(Cnd.of(clazz));
    }
    return this.getDao().fetchCount(condition);
  }

  /** 自定义sql查询clazz表. */
  default <R> long count(final String sql, final Class<R> clazz, final Object... args) {
    return this.getDao().fetchCount(sql, args);
  }

  /** 通过id获取对象. */
  default T getOne(final ID id) {
    return getOne(
        StringCnd.of(getPoJoClass())
            .eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getName(), id));
  }

  /** 通过id获取对象. */
  default T getOne(final ID id, final Fields<T> fields) {
    return getOne(
        StringCnd.of(getPoJoClass())
            .eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getName(), id),
        fields);
  }

  /** 获取单个对象. */
  default T getOne(final Condition<T, ?> condition) {
    return getOne(condition, null, condition.getClazz());
  }

  /** 获取单个对象,指定字段. */
  default T getOne(final Condition<T, ?> condition, final Fields<T> fields) {
    return getOne(condition, fields, condition.getClazz());
  }

  /** 获取单个对象,返回指定对象. */
  default <R> R getOne(final Condition<T, ?> condition, @Nonnull final Class<R> respClazz) {
    return getOne(condition, null, respClazz);
  }

  /** 获取单个对象,指定字段,返回指定对象. */
  default <R> R getOne(
      final Condition<T, ?> condition,
      @Nullable final Fields<T> fields,
      @Nonnull final Class<R> respClazz) {
    return this.getDao().fetchOne(condition, fields, respClazz);
  }

  /** 自定义sql查询clazz表. */
  default <R> R getOne(final String sql, final Class<R> clazz, final Object... args) {
    return this.getDao().fetchOne(sql, clazz, args);
  }

  /** 通过id集合查询列表. */
  default List<T> list(final Collection<Long> ids) {
    return list(ids, null);
  }

  /** 通过id集合查询列表. */
  default List<T> list(final Collection<Long> ids, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ids)) {
      return Collections.emptyList();
    }
    final Class<T> clazz = getPoJoClass();
    final Field id = EntityHelper.getEntityInfo(clazz).getIdColumn().getField();
    final Cnd<T> condition = Cnd.of(clazz).in(id, ids);
    return this.getDao().fetchAll(condition, fields, clazz);
  }

  /** 列表查询对象. */
  default List<T> list(@Nonnull final Condition<T, ?> condition) {
    return list(condition, condition.getClazz());
  }

  /** 列表查询对象,指定字段. */
  default List<T> list(@Nonnull final Condition<T, ?> condition, @Nullable final Fields<T> fields) {
    return list(condition, fields, condition.getClazz());
  }

  /** 列表查询对象,返回指定对象. */
  default <R> List<R> list(
      @Nonnull final Condition<T, ?> condition, @Nonnull final Class<R> respClazz) {
    return list(condition, null, respClazz);
  }

  /** 列表查询对象,指定字段,返回指定对象. */
  default <R> List<R> list(
      @Nonnull final Condition<T, ?> condition,
      final Fields<T> fields,
      @Nonnull final Class<R> respClazz) {
    return this.getDao().fetchAll(condition, fields, respClazz);
  }

  /** 自定义sql查询clazz表. */
  default <R> List<R> list(final String sql, final Class<R> clazz, final Object... args) {
    return this.getDao().fetchAll(sql, clazz, args);
  }

  /** 分页查询对象. */
  default Page<T> page(@Nonnull final Condition<T, ?> condition) {
    return page(condition, null, condition.getClazz());
  }

  /** 分页查询对象,指定字段. */
  default Page<T> page(@Nonnull final Condition<T, ?> condition, @Nullable final Fields<T> fields) {
    return page(condition, fields, condition.getClazz());
  }

  /**
   * 分页查询对象,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <R> Page<R> page(
      @Nonnull final Condition<T, ?> condition, @Nonnull final Class<R> respClazz) {
    return page(condition, null, respClazz);
  }

  /**
   * 分页查询对象,指定字段,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <R> Page<R> page(
      @Nonnull final Condition<T, ?> condition,
      @Nullable final Fields<T> fields,
      @Nonnull final Class<R> respClazz) {
    return this.getDao().fetchPage(condition, fields, respClazz);
  }

  /** 查询所有关联对象. */
  default <R> T populateMapping(@Nonnull T obj) {
    final List<MappingInfo> mappingInfos = EntityMapping.get(obj.getClass());
    mappingInfos.forEach(
        mappingInfo ->
            BeanUtils.setFieldValueIgnoreException(
                obj, mappingInfo.getField(), mappingInfo.getMappingObject(obj, getDao())));
    return obj;
  }

  /** 查询指定的关联对象. */
  default <R> T populateMapping(@Nonnull T obj, SetterFunction<T, R> setter) {
    setter.accept(
        obj,
        EntityMapping.get(obj.getClass(), setter.getField())
            .<R>map(mappingInfo -> mappingInfo.getMappingObject(obj, getDao()))
            .orElse(null));
    return obj;
  }

  /* 读取 */
  /* 写入 */

  /** 创建记录. */
  default int create(@Nonnull final T t) throws DataAccessException {
    return create(t, null);
  }

  /** 创建记录,仅保存指定字段. */
  default int create(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    return getDao().create(t, fields);
  }

  /**
   * 批量创建.
   *
   * @param ts the ts
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  // @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatch(final List<T> ts) throws DataAccessException {
    return getDao().createBatch(ts);
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
  // @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatch(@Nonnull final List<T> ts, final Fields<T> fields)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    return getDao().createBatch(ts, fields);
  }

  /**
   * 更新.
   *
   * @param t the t
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int update(@Nonnull final T t) throws DataAccessException {
    return update(t, null, null);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int update(@Nonnull final T object, final Condition<T, ?> condition)
      throws DataAccessException {
    return update(object, null, condition);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @param fields 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int update(@Nonnull final T object, final Fields<T> fields) throws DataAccessException {
    return update(object, fields, null);
  }

  /**
   * 更新所有符合条件的记录的指定字段.
   *
   * @param object the t
   * @param fields 更新字段
   * @param condition the condition
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int update(
      @Nonnull final T object, final Fields<T> fields, final Condition<T, ?> condition)
      throws DataAccessException {
    if (condition == null) {
      return getDao().update(object, fields);
    }
    return getDao().update(object, fields, condition);
  }

  /**
   * 批量更新.
   *
   * @param ts 要更新的数据集
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  // @Transactional(rollbackFor = Exception.class)
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
  // @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts, final Fields<T> fields)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    return getDao().updateBatch(ts, fields);
  }

  default int update(String sql, @Nonnull PreparedStatementSetter pss) throws DataAccessException {
    return getDao().update(sql, pss);
  }

  /**
   * 删除记录.
   *
   * @param id the id
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   * @see WindException
   */
  default int delete(final ID id) throws DataAccessException {
    final Cnd<T> condition = Cnd.of(getPoJoClass());
    return getDao().delete(condition.eq(EntityHelper.getEntityIdField(getPoJoClass()), id));
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
    final Cnd<T> condition = Cnd.of(getPoJoClass());
    condition.in(EntityHelper.getEntityIdField(getPoJoClass()), ids);
    final int affectRow = getDao().delete(condition);
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
    return getDao().delete(condition);
  }

  /* 写入 */

  Dao getDao();

  /** 获取service操作的实体. */
  Class<T> getPoJoClass();
}
