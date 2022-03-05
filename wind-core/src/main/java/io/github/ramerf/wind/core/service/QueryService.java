package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public interface QueryService<T, ID extends Serializable> extends InterService<T, ID> {

  default long count(@Nullable final Cnd<T, ?, ?> cnd) {
    if (cnd == null) {
      Class<T> clazz = getPoJoClass();
      return getQuery(clazz).select(null).where(LambdaCondition.of(clazz)).fetchCount(clazz);
    }
    return getQuery(cnd.getClazz())
        .select(null)
        .where(cnd.getCondition())
        .fetchCount(cnd.getClazz());
  }

  /** 通过id获取对象. */
  default T getOne(final ID id) {
    return getOne(
        StringCnds.of(getPoJoClass())
            .eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getName(), id));
  }

  /** 通过id获取对象. */
  default T getOne(final ID id, final Fields<T> fields) {
    return getOne(
        StringCnds.of(getPoJoClass())
            .eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getName(), id),
        fields);
  }

  /** 获取单个对象. */
  default T getOne(final Cnd<T, ?, ?> cnd) {
    return getOne(cnd, null, cnd.getClazz());
  }

  /** 获取单个对象,指定字段. */
  default T getOne(final Cnd<T, ?, ?> cnd, final Fields<T> fields) {
    return getOne(cnd, fields, cnd.getClazz());
  }

  /** 获取单个对象,返回指定对象. */
  default <R> R getOne(final Cnd<T, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return getOne(cnd, null, respClazz);
  }

  /** 获取单个对象,指定字段,返回指定对象. */
  default <R> R getOne(
      final Cnd<T, ?, ?> cnd, @Nullable final Fields<T> fields, @Nonnull final Class<R> respClazz) {
    return getQuery(cnd.getClazz())
        .select(fields)
        .where(cnd.getCondition())
        .pageable(cnd.getPageRequest())
        .fetchOne(respClazz);
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
    return getQuery()
        .select(fields)
        .where(
            LambdaCondition.of(clazz)
                .in(EntityHelper.getEntityInfo(clazz).getIdColumn().getField(), ids))
        .fetchAll(clazz);
  }

  /** 列表查询对象. */
  default List<T> list(@Nonnull final Cnd<T, ?, ?> cnd) {
    return list(cnd, cnd.getClazz());
  }

  /** 列表查询对象,指定字段. */
  default List<T> list(@Nonnull final Cnd<T, ?, ?> cnd, @Nullable final Fields<T> fields) {
    return list(cnd, fields, cnd.getClazz());
  }

  /** 列表查询对象,返回指定对象. */
  default <R> List<R> list(@Nonnull final Cnd<T, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return list(cnd, null, respClazz);
  }

  /** 列表查询对象,指定字段,返回指定对象. */
  default <R> List<R> list(
      @Nonnull final Cnd<T, ?, ?> cnd, final Fields<T> fields, @Nonnull final Class<R> respClazz) {
    return getQuery(cnd.getClazz())
        .select(fields)
        .where(cnd.getCondition())
        .pageable(cnd.getPageRequest())
        .fetchAll(respClazz);
  }

  /** 分页查询对象. */
  default Page<T> page(@Nonnull final Cnd<T, ?, ?> cnd) {
    return page(cnd, null, cnd.getClazz());
  }

  /** 分页查询对象,指定字段. */
  default Page<T> page(@Nonnull final Cnd<T, ?, ?> cnd, @Nullable final Fields<T> fields) {
    return page(cnd, fields, cnd.getClazz());
  }

  /**
   * 分页查询对象,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <R> Page<R> page(@Nonnull final Cnd<T, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return page(cnd, null, respClazz);
  }

  /**
   * 分页查询对象,指定字段,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <R> Page<R> page(
      @Nonnull final Cnd<T, ?, ?> cnd,
      @Nullable final Fields<T> fields,
      @Nonnull final Class<R> respClazz) {
    return getQuery(cnd.getClazz())
        .select(fields)
        .where(cnd.getCondition())
        .pageable(cnd.getPageRequest())
        .fetchPage(respClazz);
  }

  /** 查询所有关联对象. */
  default <R> T populateMapping(@Nonnull T obj) {
    final List<MappingInfo> mappingInfos = EntityMapping.get(obj.getClass());
    mappingInfos.forEach(
        mappingInfo ->
            BeanUtils.setFieldValueIgnoreException(
                obj, mappingInfo.getField(), mappingInfo.getMappingObject(obj)));
    return obj;
  }

  /** 查询指定的关联对象. */
  default <R> T populateMapping(@Nonnull T obj, SetterFunction<T, R> setter) {
    setter.accept(
        obj,
        EntityMapping.get(obj.getClass(), setter.getField())
            .<R>map(mappingInfo -> mappingInfo.getMappingObject(obj))
            .orElse(null));
    return obj;
  }

  /** 自定义sql查询clazz表. */
  default <R> R fetchOneBySql(final String sql, final Class<R> clazz, final Object... args) {
    return getQuery(clazz).fetchOneBySql(sql, clazz, args);
  }

  /** 自定义sql查询clazz表. */
  default <R> List<R> fetchListBySql(final String sql, final Class<R> clazz, final Object... args) {
    return getQuery(clazz).fetchListBySql(sql, clazz, args);
  }

  /** 自定义sql查询clazz表. */
  default <R> long countBySql(final String sql, final Class<R> clazz, final Object... args) {
    return getQuery(clazz).countBySql(sql, args);
  }
}
