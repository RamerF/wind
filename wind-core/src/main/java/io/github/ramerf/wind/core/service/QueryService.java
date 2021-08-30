package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

public interface QueryService<T, ID extends Serializable> extends InterService<T, ID> {

  default <E> long count(@Nullable final Cnd<E, ?, ?> cnd) {
    if (cnd == null) {
      return count(Cnds.of(getPoJoClass()));
    }
    return getQuery(cnd.getClazz())
        .select(QueryColumn.of(cnd.getClazz()))
        .where(cnd.getCondition())
        .fetchCount(cnd.getClazz());
  }

  /** 通过id获取对象. */
  default T getById(final ID id) {
    return getOne(
        StringCnds.of(getPoJoClass())
            .eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getName(), id));
  }

  /** 获取单个任意对象. */
  default <E> E getOne(final Cnd<E, ?, ?> cnd) {
    return getOne(cnd, null, cnd.getClazz());
  }

  /** 获取单个任意对象,指定字段. */
  default <E> E getOne(final Cnd<E, ?, ?> cnd, @Nullable final QueryColumn<E> queryColumn) {
    return getOne(cnd, queryColumn, cnd.getClazz());
  }

  /** 获取单个任意对象,返回指定对象. */
  default <E, R> R getOne(final Cnd<E, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return getOne(cnd, null, respClazz);
  }

  /** 获取单个任意对象,指定字段,返回指定对象. */
  default <E, R> R getOne(
      final Cnd<E, ?, ?> cnd,
      @Nullable final QueryColumn<E> queryColumn,
      @Nonnull final Class<R> respClazz) {
    return getQuery(cnd.getClazz())
        .select(queryColumn == null ? QueryColumn.of(cnd.getClazz()) : queryColumn)
        .where(cnd.getCondition())
        .pageable(cnd.getPages())
        .fetchOne(respClazz);
  }

  /** 通过id集合查询列表. */
  default List<T> listByIds(final Collection<Long> ids) {
    if (CollectionUtils.isEmpty(ids)) {
      return Collections.emptyList();
    }
    final Class<T> clazz = getPoJoClass();
    return getQuery()
        .select(QueryColumn.of(clazz))
        .where(
            LambdaCondition.of(clazz)
                .in(EntityHelper.getEntityInfo(clazz).getIdColumn().getField(), ids))
        .fetchAll(clazz);
  }

  /** 列表查询任意对象. */
  default <E> List<E> list(@Nonnull final Cnd<E, ?, ?> cnd) {
    return list(cnd, cnd.getClazz());
  }

  /** 列表查询任意对象,返回指定对象. */
  default <E, R> List<R> list(@Nonnull final Cnd<E, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return list(cnd, null, respClazz);
  }

  /** 列表查询任意对象,指定字段. */
  default <E> List<E> list(
      @Nonnull final Cnd<E, ?, ?> cnd, @Nullable final QueryColumn<E> queryColumn) {
    return list(cnd, queryColumn, cnd.getClazz());
  }

  /** 列表查询任意对象,指定字段,返回指定对象. */
  default <E, R> List<R> list(
      @Nonnull final Cnd<E, ?, ?> cnd,
      @Nullable final QueryColumn<E> queryColumn,
      @Nonnull final Class<R> respClazz) {
    return getQuery(cnd.getClazz())
        .select(queryColumn == null ? QueryColumn.of(cnd.getClazz()) : queryColumn)
        .where(cnd.getCondition())
        .pageable(cnd.getPages())
        .fetchAll(respClazz);
  }

  /** 分页查询任意对象. */
  default <E> Page<E> page(@Nonnull final Cnd<E, ?, ?> cnd) {
    return page(cnd, null, cnd.getClazz());
  }

  /** 分页查询任意对象,指定字段. */
  default <E> Page<E> page(
      @Nonnull final Cnd<E, ?, ?> cnd, @Nullable final QueryColumn<E> queryColumn) {
    return page(cnd, queryColumn, cnd.getClazz());
  }

  /**
   * 分页查询任意对象,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <E, R> Page<R> page(@Nonnull final Cnd<E, ?, ?> cnd, @Nonnull final Class<R> respClazz) {
    return page(cnd, null, respClazz);
  }

  /**
   * 分页查询任意对象,指定字段,返回指定对象.
   *
   * @param respClazz 返回对象
   */
  default <E, R> Page<R> page(
      @Nonnull final Cnd<E, ?, ?> cnd,
      @Nullable final QueryColumn<E> queryColumn,
      @Nonnull final Class<R> respClazz) {
    final Pages pageable = cnd.getPages();
    if (pageable == null) {
      return new PageImpl<>(Collections.emptyList());
    }
    return getQuery(cnd.getClazz())
        .select(queryColumn == null ? QueryColumn.of(cnd.getClazz()) : queryColumn)
        .where(cnd.getCondition())
        .pageable(cnd.getPages())
        .fetchPage(respClazz);
  }

  /** 查询关联对象. */
  default <R> T queryMapping(@Nonnull T t, IConsumer<T, R> setField) {
    setField.accept(
        t,
        EntityMapping.get(t.getClass(), setField.getField())
            .<R>map(mappingInfo -> mappingInfo.getMappingObject(t))
            .orElse(null));
    return t;
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
