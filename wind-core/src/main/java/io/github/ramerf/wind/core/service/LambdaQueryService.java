// package io.github.ramerf.wind.core.service;
//
// import io.github.ramerf.wind.core.condition.*;
// import io.github.ramerf.wind.core.function.IConsumer;
// import io.github.ramerf.wind.core.helper.EntityHelper;
// import io.github.ramerf.wind.core.mapping.EntityMapping;
// import io.github.ramerf.wind.core.util.CollectionUtils;
// import java.io.Serializable;
// import java.util.*;
// import java.util.function.Consumer;
// import javax.annotation.Nonnull;
// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;
// import org.springframework.data.domain.*;
// import org.springframework.data.domain.Sort.Order;
//
// /**
//  * 公共查询接口.
//  *
//  * <pre>
//  * <h2>
//  *   <font color="yellow">注意: 所有的方法忽略逻辑删除记录.</code></font>
//  * </h2>
//  * </pre>
//  *
//  * @param <T> the type parameter
//  * @author ramer
//  * @since 2020 /1/5
//  */
// public interface LambdaQueryService<T, ID extends Serializable> extends InterService<T, ID> {
//   /** The constant log. */
//   Logger log = LoggerFactory.getLogger(LambdaQueryService.class);
//
//   /**
//    * 总记录数(逻辑未删除).
//    *
//    * @return long long
//    */
//   default long count() {
//     return count(null, null);
//   }
//
//   /**
//    * 给定条件count(1).
//    *
//    * @param condition the condition
//    * @return long long
//    */
//   default long count(final Consumer<LambdaCondition<T>> condition) {
//     return count(null, condition);
//   }
//
//   /**
//    * 给定条件count指定列.
//    *
//    * @param queryConsumer the queryConsumer
//    * @param conditionConsumer the conditionConsumer
//    * @return long long
//    */
//   default long count(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer) {
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, conditionConsumer,
// this);
//     return getQuery()
//         .select(queryBound.queryColumn)
//         .where(queryBound.condition)
//         .fetchCount(getPoJoClass());
//   }
//
//   /**
//    * Gets by id.
//    *
//    * @param id the id
//    * @return the T
//    */
//   default T getById(final ID id) {
//     return getOne(
//         null,
//         condition ->
//             condition.eq(EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getField(),
// id),
//         getPoJoClass());
//   }
//
//   /**
//    * 获取单个对象.
//    *
//    * @param consumer 查询条件
//    * @return the ones
//    */
//   default T getOne(final Consumer<LambdaCondition<T>> consumer) {
//     return getOne(null, consumer, getPoJoClass());
//   }
//
//   /**
//    * 获取单个对象.
//    *
//    * @param <R> 返回对象
//    * @param queryConsumer 查询列
//    * @param clazz 返回对象
//    * @return the ones
//    */
//   default <R> R getOne(
//       final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
//     return getOne(queryConsumer, null, clazz);
//   }
//
//   /**
//    * 获取单个对象.
//    *
//    * @param query 查询字段
//    * @param condition 查询条件
//    * @return T one
//    */
//   default T getOne(
//       final Consumer<QueryColumn<T>> query, final Consumer<LambdaCondition<T>> condition) {
//     return getOne(query, condition, getPoJoClass());
//   }
//
//   /**
//    * 获取单个对象.
//    *
//    * @param <R> the type parameter
//    * @param queryConsumer 查询列
//    * @param conditionConsumer 查询条件
//    * @param clazz 返回对象
//    * @return the one
//    */
//   default <R> R getOne(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       @Nonnull final Class<R> clazz) {
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, conditionConsumer,
// this);
//     return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchOne(clazz);
//   }
//
//   /**
//    * 自定义sql查询.
//    *
//    * @param args 占位符?参数
//    */
//   default <R> R getOne(final String sql, @Nonnull final Class<R> respClazz, final Object... args)
// {
//     return getQuery().fetchOneBySql(sql, respClazz, args);
//   }
//
//   /** 查询关联对象. */
//   default <R> T queryMapping(@Nonnull T t, IConsumer<T, R> setField) {
//     setField.accept(
//         t,
//         EntityMapping.get(t.getClass(), setField.getField())
//             .<R>map(mappingInfo -> mappingInfo.getMappingObject(t))
//             .orElse(null));
//     return t;
//   }
//
//   /**
//    * 通过id集合查询列表.
//    *
//    * @param ids the ids
//    * @return the list
//    */
//   default List<T> listByIds(final Collection<Long> ids) {
//     if (CollectionUtils.isEmpty(ids)) {
//       return Collections.emptyList();
//     }
//     final QueryBound<T, ID> queryBound =
//         QueryBound.consume(
//             null,
//             condition ->
//                 condition.in(
//                     EntityHelper.getEntityInfo(getPoJoClass()).getIdColumn().getField(), ids),
//             this);
//     return getQuery()
//         .select(queryBound.queryColumn)
//         .where(queryBound.condition)
//         .fetchAll(getPoJoClass());
//   }
//
//   /**
//    * 查询列表.
//    *
//    * @param consumer the consumer
//    * @return the list
//    */
//   default List<T> list(final Consumer<LambdaCondition<T>> consumer) {
//     return list(null, consumer, getPoJoClass());
//   }
//
//   /**
//    * 查询clazz列表.
//    *
//    * @param <R> 返回对象
//    * @param queryConsumer 查询列
//    * @param clazz 返回对象class
//    * @return the list
//    */
//   default <R> List<R> list(
//       final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
//     return list(queryConsumer, null, clazz);
//   }
//
//   /**
//    * 查询指定字段.
//    *
//    * @param queryConsumer 查询列
//    * @param conditionConsumer 查询条件
//    * @return the list
//    */
//   default List<T> list(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer) {
//     return list(queryConsumer, conditionConsumer, getPoJoClass());
//   }
//
//   /**
//    * 查询指定字段,返回<code>clazz</code>列表.
//    *
//    * @param <R> 返回对象
//    * @param queryConsumer 查询字段
//    * @param conditionConsumer 查询条件
//    * @param clazz 返回对象
//    * @return the list
//    */
//   default <R> List<R> list(
//       Consumer<QueryColumn<T>> queryConsumer,
//       Consumer<LambdaCondition<T>> conditionConsumer,
//       @Nonnull final Class<R> clazz) {
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, conditionConsumer,
// this);
//     return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchAll(clazz);
//   }
//
//   /**
//    * 获取某页列表数据.
//    *
//    * @param conditionConsumer 查询条件
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @return 对象列表 list
//    */
//   default List<T> list(
//       final Consumer<LambdaCondition<T>> conditionConsumer, final int page, final int size) {
//     return list(null, conditionConsumer, page, size, null, getPoJoClass());
//   }
//
//   /**
//    * 获取某页列表数据.
//    *
//    * @param conditionConsumer 查询条件
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @return 对象列表 list
//    */
//   default List<T> list(
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn) {
//     return list(null, conditionConsumer, page, size, sortColumn, getPoJoClass());
//   }
//
//   /**
//    * 获取某页列表数据,返回指定对象.
//    *
//    * @param <R> the type parameter
//    * @param queryConsumer 查询列
//    * @param conditionConsumer 查询条件
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
//    * @param clazz 返回对象
//    * @return clazz对象列表 list
//    */
//   default <R> List<R> list(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn,
//       @Nonnull final Class<R> clazz) {
//     final PageRequest pageable = pageRequest(page, size, sortColumn);
//     if (Objects.isNull(pageable)) {
//       return Collections.emptyList();
//     }
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, conditionConsumer,
// this);
//     return getQuery()
//         .select(queryBound.queryColumn)
//         .where(queryBound.condition)
//         .fetchAll(clazz, pageable);
//   }
//
//   /** 查询列表. */
//   default List<T> listAll() {
//     return listAll(null, getPoJoClass());
//   }
//
//   /**
//    * 查询列表.
//    *
//    * @param queryConsumer 查询列
//    * @return the list
//    */
//   default List<T> listAll(final Consumer<QueryColumn<T>> queryConsumer) {
//     return listAll(queryConsumer, getPoJoClass());
//   }
//
//   /**
//    * 查询列表,返回指定对象.
//    *
//    * @param <R> 方法对象
//    * @param queryConsumer 查询列
//    * @param clazz 返回对象
//    * @return the list
//    */
//   default <R> List<R> listAll(
//       final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, null, this);
//     return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchAll(clazz);
//   }
//
//   /**
//    * 自定义sql查询.
//    *
//    * @param args 占位符?参数
//    */
//   default <R> List<R> listAll(
//       final String sql, @Nonnull final Class<R> respClazz, final Object... args) {
//     return getQuery().fetchAllBySql(sql, respClazz, args);
//   }
//
//   /**
//    * 查询分页.
//    *
//    * @param conditionConsumer 查询条件
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @return 分页数据 page
//    */
//   default Page<T> page(
//       final Consumer<LambdaCondition<T>> conditionConsumer, final int page, final int size) {
//     return page(null, conditionConsumer, page, size, null, getPoJoClass());
//   }
//
//   /**
//    * 查询分页.
//    *
//    * @param conditionConsumer 查询条件
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则
//    * @return 分页数据 page
//    */
//   default Page<T> page(
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn) {
//     return page(null, conditionConsumer, page, size, sortColumn, getPoJoClass());
//   }
//
//   /**
//    * Page page.
//    *
//    * @param <R> the type parameter
//    * @param queryConsumer 查询列
//    * @param page 当前页码,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则
//    * @param clazz the clazz
//    * @return the page
//    */
//   default <R> Page<R> page(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn,
//       @Nonnull final Class<R> clazz) {
//     return page(queryConsumer, null, page, size, sortColumn, clazz);
//   }
//
//   /**
//    * 查询分页.
//    *
//    * @param queryConsumer 查询列
//    * @param conditionConsumer 查询条件
//    * @param page 当前页,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则
//    * @return the page
//    */
//   default Page<T> page(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn) {
//     return page(queryConsumer, conditionConsumer, page, size, sortColumn, getPoJoClass());
//   }
//
//   /**
//    * Page page.
//    *
//    * @param <R> the type parameter
//    * @param queryConsumer 查询列
//    * @param conditionConsumer 查询条件
//    * @param page 当前页,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
//    * @param clazz the clazz
//    * @return the page
//    */
//   default <R> Page<R> page(
//       final Consumer<QueryColumn<T>> queryConsumer,
//       final Consumer<LambdaCondition<T>> conditionConsumer,
//       final int page,
//       final int size,
//       final SortColumn sortColumn,
//       @Nonnull final Class<R> clazz) {
//     final PageRequest pageable = pageRequest(page, size, sortColumn);
//     if (Objects.isNull(pageable)) {
//       return new PageImpl<>(Collections.emptyList());
//     }
//     final QueryBound<T, ID> queryBound = QueryBound.consume(queryConsumer, conditionConsumer,
// this);
//     return getQuery()
//         .select(queryBound.queryColumn)
//         .where(queryBound.condition)
//         .fetchPage(clazz, pageable);
//   }
//
//   /**
//    * 获取分页对象,以更新时间倒序.
//    *
//    * @param page 当前页,从1开始
//    * @param size 每页大小
//    * @return the page request
//    */
//   default PageRequest pageRequest(final int page, final int size) {
//     return pageRequest(page, size, Sort.unsorted());
//   }
//
//   /**
//    * 获取分页对象,支持排序.
//    *
//    * @param page 当前页,从1开始
//    * @param size 每页大小
//    * @param sortColumn 排序规则,null时以更新时间倒序
//    * @return the page request
//    * @see SortColumn
//    * @see Order
//    * @see Sort
//    */
//   default PageRequest pageRequest(final int page, final int size, final SortColumn sortColumn) {
//     return pageRequest(page, size, (sortColumn == null ? new SortColumn() :
// sortColumn).getSort());
//   }
//
//   /**
//    * 获取分页对象,支持排序.
//    *
//    * @param page 当前页,从1开始
//    * @param size 每页大小
//    * @param sort 排序规则
//    * @return the page request
//    */
//   default PageRequest pageRequest(final int page, final int size, final Sort sort) {
//     if ((page < 1 || size < 0) && page != size) {
//       return null;
//     }
//     return page == -1
//         ? PageRequest.of(0, Integer.MAX_VALUE, sort)
//         : PageRequest.of(page - 1, size > 0 ? size : 10, sort);
//   }
// }
