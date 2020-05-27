package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.*;
import org.springframework.data.domain.Sort.Order;

/**
 * 公共查询接口.
 *
 * <pre>
 * <h2>
 *   <font color="yellow">注意: 所有的方法忽略已删除记录<code>{@link AbstractEntityPoJo#getIsDelete()}=false.</code></font>
 * </h2>
 * </pre>
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings({"unused"})
public interface QueryService<T extends AbstractEntityPoJo> extends InterService<T> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(QueryService.class);

  /**
   * 总记录数(逻辑未删除).
   *
   * @return long long
   */
  default long count() {
    return count(null, null);
  }

  /**
   * 给定条件count(1).
   *
   * @param condition the condition
   * @return long long
   */
  default long count(final Consumer<ICondition<T>> condition) {
    return count(null, condition);
  }

  /**
   * 给定条件count指定列.
   *
   * @param queryConsumer the queryConsumer
   * @param conditionConsumer the conditionConsumer
   * @return long long
   */
  default long count(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer) {
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, conditionConsumer, this);
    return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchCount();
  }

  /**
   * Gets by id.
   *
   * @param id the id
   * @return the T
   */
  default T getById(final long id) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    final ICondition<T> condition = queryColumn.getCondition().eq(AbstractEntityPoJo::setId, id);
    return getQuery().select(queryColumn).where(condition).fetchOne(getPoJoClass());
  }

  /**
   * 获取单个PoJo对象.
   *
   * @param consumer 查询条件
   * @return the ones
   */
  default T getOne(final Consumer<ICondition<T>> consumer) {
    return getOne(null, consumer, getPoJoClass());
  }

  /**
   * 获取单个对象.
   *
   * @param <R> 返回对象
   * @param queryConsumer 查询列
   * @param clazz 返回对象
   * @return the ones
   */
  default <R> R getOne(
      final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
    return getOne(queryConsumer, null, clazz);
  }

  /**
   * 获取单个PoJo对象.
   *
   * @param query 查询字段
   * @param condition 查询条件
   * @return T one
   */
  default T getOne(final Consumer<QueryColumn<T>> query, final Consumer<ICondition<T>> condition) {
    return getOne(query, condition, getPoJoClass());
  }

  /**
   * 获取单个对象.
   *
   * @param <R> the type parameter
   * @param queryConsumer 查询列
   * @param conditionConsumer 查询条件
   * @param clazz 返回对象
   * @return the one
   */
  default <R> R getOne(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer,
      @Nonnull final Class<R> clazz) {
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, conditionConsumer, this);
    return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchOne(clazz);
  }

  /**
   * 通过id集合查询PoJo列表.
   *
   * @param ids the ids
   * @return the list
   */
  default List<T> listByIds(final Collection<Long> ids) {
    if (CollectionUtils.isEmpty(ids)) {
      return Collections.emptyList();
    }
    final QueryBound<T> queryBound =
        QueryBound.consume(null, condition -> condition.in(AbstractEntityPoJo::setId, ids), this);
    return getQuery()
        .select(queryBound.queryColumn)
        .where(queryBound.condition)
        .fetchAll(getPoJoClass());
  }

  /**
   * 查询PoJo列表.
   *
   * @param consumer the consumer
   * @return the list
   */
  default List<T> list(final Consumer<ICondition<T>> consumer) {
    return list(null, consumer, getPoJoClass());
  }

  /**
   * 查询clazz列表.
   *
   * @param <R> 返回对象
   * @param queryConsumer 查询列
   * @param clazz 返回对象class
   * @return the list
   */
  default <R> List<R> list(
      final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
    return list(queryConsumer, null, clazz);
  }

  /**
   * 查询指定字段,返回PoJo列表.
   *
   * @param queryConsumer 查询列
   * @param conditionConsumer 查询条件
   * @return the list
   */
  default List<T> list(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer) {
    return list(queryConsumer, conditionConsumer, getPoJoClass());
  }

  /**
   * 查询指定字段,返回<code>clazz</code>列表.
   *
   * @param <R> 返回对象
   * @param queryConsumer 查询字段
   * @param conditionConsumer 查询条件
   * @param clazz 返回对象
   * @return the list
   */
  default <R> List<R> list(
      Consumer<QueryColumn<T>> queryConsumer,
      Consumer<ICondition<T>> conditionConsumer,
      @Nonnull final Class<R> clazz) {
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, conditionConsumer, this);
    return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchAll(clazz);
  }

  /**
   * 获取某页列表数据,返回PoJo对象.
   *
   * @param conditionConsumer 查询条件
   * @param page 当前页码,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @return PoJo对象列表 list
   */
  default List<T> list(
      final Consumer<ICondition<T>> conditionConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn) {
    return list(null, conditionConsumer, page, size, sortColumn, getPoJoClass());
  }

  /**
   * 获取某页列表数据,返回指定对象.
   *
   * @param <R> the type parameter
   * @param queryConsumer 查询列
   * @param conditionConsumer 查询条件
   * @param page 当前页码,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @param clazz 返回对象
   * @return clazz对象列表 list
   */
  default <R> List<R> list(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn,
      @Nonnull final Class<R> clazz) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return Collections.emptyList();
    }
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, conditionConsumer, this);
    return getQuery()
        .select(queryBound.queryColumn)
        .where(queryBound.condition)
        .fetchAll(clazz, pageable);
  }

  /**
   * 查询PoJo列表.
   *
   * @param queryConsumer 查询列
   * @return the list
   */
  default List<T> listAll(final Consumer<QueryColumn<T>> queryConsumer) {
    return listAll(queryConsumer, getPoJoClass());
  }

  /**
   * 查询列表,返回指定对象.
   *
   * @param <R> 方法对象
   * @param queryConsumer 查询列
   * @param clazz 返回对象
   * @return the list
   */
  default <R> List<R> listAll(
      final Consumer<QueryColumn<T>> queryConsumer, @Nonnull final Class<R> clazz) {
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, null, this);
    return getQuery().select(queryBound.queryColumn).where(queryBound.condition).fetchAll(clazz);
  }

  /**
   * 查询分页.
   *
   * @param conditionConsumer 查询条件
   * @param page 当前页码,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @return PoJo分页数据 page
   */
  default Page<T> page(
      final Consumer<ICondition<T>> conditionConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn) {
    return page(null, conditionConsumer, page, size, sortColumn, getPoJoClass());
  }

  /**
   * Page page.
   *
   * @param <R> the type parameter
   * @param queryConsumer 查询列
   * @param page 当前页码,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @param clazz the clazz
   * @return the page
   */
  default <R> Page<R> page(
      final Consumer<QueryColumn<T>> queryConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn,
      @Nonnull final Class<R> clazz) {
    return page(queryConsumer, null, page, size, sortColumn, clazz);
  }

  /**
   * 查询分页,返回PoJo对象.
   *
   * @param queryConsumer 查询列
   * @param conditionConsumer 查询条件
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @return the page
   */
  default Page<T> page(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn) {
    return page(queryConsumer, conditionConsumer, page, size, sortColumn, getPoJoClass());
  }

  /**
   * Page page.
   *
   * @param <R> the type parameter
   * @param queryConsumer 查询列
   * @param conditionConsumer 查询条件
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则{@link SortColumn},null时按update_time倒序
   * @param clazz the clazz
   * @return the page
   */
  default <R> Page<R> page(
      final Consumer<QueryColumn<T>> queryConsumer,
      final Consumer<ICondition<T>> conditionConsumer,
      final int page,
      final int size,
      final SortColumn sortColumn,
      @Nonnull final Class<R> clazz) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return new PageImpl<>(Collections.emptyList());
    }
    final QueryBound<T> queryBound = QueryBound.consume(queryConsumer, conditionConsumer, this);
    return getQuery()
        .select(queryBound.queryColumn)
        .where(queryBound.condition)
        .fetchPage(clazz, pageable);
  }

  /**
   * 获取分页对象,以更新时间倒序.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @return the page request
   */
  default PageRequest pageRequest(final int page, final int size) {
    return pageRequest(page, size, (SortColumn) null);
  }

  /**
   * 获取分页对象,支持排序.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则,null时以更新时间倒序
   * @return the page request
   * @see SortColumn
   * @see Order
   * @see Sort
   */
  default PageRequest pageRequest(final int page, final int size, final SortColumn sortColumn) {
    // 默认以update_time倒序
    return pageRequest(
        page,
        size,
        Objects.nonNull(sortColumn)
            ? sortColumn.getSort()
            : SortColumn.by(AbstractEntityPoJo::getCreateTime, SortColumn.Order.DESC).getSort());
  }

  /**
   * 获取分页对象,支持排序.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sort 排序规则
   * @return the page request
   */
  default PageRequest pageRequest(final int page, final int size, final Sort sort) {
    if ((page < 1 || size < 0) && page != size) {
      return null;
    }
    return page == -1
        ? PageRequest.of(0, Integer.MAX_VALUE, sort)
        : PageRequest.of(page - 1, size > 0 ? size : Constant.DEFAULT_PAGE_SIZE, sort);
  }

  class QueryBound<T extends AbstractEntityPoJo> {
    protected Consumer<QueryColumn<T>> queryConsumer;
    protected Consumer<ICondition<T>> conditionConsumer;
    protected QueryColumn<T> queryColumn;
    protected ICondition<T> condition;
    protected InterService<T> service;

    /**
     * 填充{@code queryConsumer} 和 {@code conditionConsumer}.
     *
     * @param queryConsumer 查询列
     * @param conditionConsumer 查询条件
     * @param service {@code BaseService}
     * @param <T> the type parameter
     * @return {@code QueryBound}
     */
    public static <T extends AbstractEntityPoJo> QueryBound<T> consume(
        final Consumer<QueryColumn<T>> queryConsumer,
        final Consumer<ICondition<T>> conditionConsumer,
        final InterService<T> service) {
      QueryBound<T> queryBound = new QueryBound<>();
      queryBound.queryConsumer = queryConsumer;
      queryBound.conditionConsumer = conditionConsumer;
      queryBound.service = service;
      final QueryColumn<T> queryColumn = service.getQueryColumn();
      Optional.ofNullable(queryConsumer).ifPresent(o -> o.accept(queryColumn));
      final ICondition<T> condition = queryColumn.getCondition();
      Optional.ofNullable(conditionConsumer).ifPresent(o -> o.accept(condition));
      queryBound.queryColumn = queryColumn;
      queryBound.condition = condition;
      return queryBound;
    }
  }
}
