package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.util.*;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.*;
import org.springframework.data.domain.Sort.Order;

import static io.github.ramerf.wind.core.util.EntityUtils.getPoJoClass;

/**
 * 公共查询接口,<b>注意: 所有的方法忽略已删除记录.</b>
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings("unused")
public interface QueryService<T extends AbstractEntityPoJo> extends InterService<T> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(QueryService.class);

  /**
   * 条件is_delete=false的总记录数.
   *
   * @return long long
   */
  default long count() {
    final QueryColumn<T> queryColumn = getQueryColumn();
    return getQuery().select(queryColumn).where(queryColumn.getCondition()).fetchCount();
  }

  /**
   * 给定条件,执行count 指定列.
   *
   * @param query the query
   * @param condition the condition
   * @return long long
   */
  default long count(Consumer<QueryColumn<T>> query, Consumer<Condition<T>> condition) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    Optional.ofNullable(query).ifPresent(q -> q.accept(queryColumn));
    Optional.ofNullable(condition).ifPresent(cond -> cond.accept(queryColumn.getCondition()));
    return getQuery().select(queryColumn).where(queryColumn.getCondition()).fetchCount();
  }

  /**
   * 给定条件,执行count(1).
   *
   * @param condition the condition
   * @return long long
   */
  default long count(Consumer<Condition<T>> condition) {
    return count(null, condition);
  }

  /**
   * 获取单个PoJo对象
   *
   * @param consumer the consumer
   * @return the one
   */
  @Deprecated
  default T getOne(Consumer<QueryColumn<T>> consumer) {
    return getOne(consumer, getPoJoClass(this));
  }

  /**
   * 获取单个对象,可指定查询字段.
   *
   * @param <R> the type parameter
   * @param query 查询字段+条件
   * @param clazz 返回对象
   * @return the one
   * @see #getOne(Consumer, Consumer, Class) #getOne(Consumer, Consumer, Class)#getOne(Consumer,
   *     Consumer, Class)
   */
  default <R> R getOne(Consumer<QueryColumn<T>> query, Class<R> clazz) {
    return getOne(query, null, clazz);
  }

  /**
   * 获取单个对象.
   *
   * @param query 指定查询字段
   * @param condition 查询条件
   * @return {@link T}
   */
  default T getOne(Consumer<QueryColumn<T>> query, Consumer<Condition<T>> condition) {
    return getOne(query, condition, getPoJoClass(this));
  }

  /**
   * 获取单个对象.
   *
   * @param <R> the type parameter
   * @param query the query
   * @param condition the condition
   * @param clazz the clazz
   * @return the one
   */
  default <R> R getOne(
      Consumer<QueryColumn<T>> query, Consumer<Condition<T>> condition, Class<R> clazz) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    query.accept(queryColumn);
    final Condition<T> cond = queryColumn.getCondition();
    Optional.ofNullable(condition).ifPresent(consumer -> consumer.accept(cond));
    return getQuery().select(queryColumn).where(cond).fetchOne(clazz);
  }

  /**
   * 获取单个PoJo对象.
   *
   * @param consumer 查询条件
   * @return the ones
   */
  default T getOnes(Consumer<Condition<T>> consumer) {
    return getOnes(consumer, getPoJoClass(this));
  }

  /**
   * 获取单个对象.
   *
   * @param <R> 返回对象
   * @param consumer 查询条件
   * @param clazz 返回对象
   * @return the ones
   */
  default <R> R getOnes(Consumer<Condition<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    final Condition<T> condition = queryColumn.getCondition();
    consumer.accept(condition);
    return getQuery().select(queryColumn).where(condition).fetchOne(clazz);
  }

  /**
   * 查询PoJo列表.
   *
   * @param consumer the consumer
   * @return the list
   */
  default List<T> list(Consumer<QueryColumn<T>> consumer) {
    return list(consumer, getPoJoClass(this));
  }

  /**
   * 查询列表,返回指定对象.
   *
   * @param <R> 方法对象
   * @param consumer 查询条件
   * @param clazz 返回对象
   * @return the list
   */
  default <R> List<R> list(Consumer<QueryColumn<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    consumer.accept(queryColumn);
    return getQuery().select(queryColumn).where(queryColumn.getCondition()).fetchAll(clazz);
  }

  /**
   * 查询指定字段,返回<code>PoJo</code>列表.
   *
   * @param query 查询字段
   * @param condition 查询条件
   * @return the list
   */
  default List<T> list(Consumer<QueryColumn<T>> query, Consumer<Condition<T>> condition) {
    return list(query, condition, getPoJoClass(this));
  }

  /**
   * 查询指定字段,返回<code>clazz</code>列表.
   *
   * @param <R> 返回对象
   * @param query 查询字段
   * @param condition 查询条件
   * @param clazz 返回对象
   * @return the list
   */
  default <R> List<R> list(
      Consumer<QueryColumn<T>> query, Consumer<Condition<T>> condition, final Class<R> clazz) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    query.accept(queryColumn);
    Optional.ofNullable(condition)
        .ifPresent(consumer -> consumer.accept(queryColumn.getCondition()));
    return getQuery().select(queryColumn).where(queryColumn.getCondition()).fetchAll(clazz);
  }

  /**
   * 查询PoJo列表.
   *
   * @param consumer the consumer
   * @return the list
   */
  default List<T> lists(Consumer<Condition<T>> consumer) {
    return lists(consumer, getPoJoClass(this));
  }

  /**
   * 查询<code>clazz</code>列表.
   *
   * @param <R> 返回对象
   * @param consumer the consumer
   * @param clazz 返回对象class
   * @return the list
   */
  default <R> List<R> lists(Consumer<Condition<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    final Condition<T> condition = queryColumn.getCondition();
    Optional.ofNullable(consumer).ifPresent(con -> con.accept(condition));
    return getQuery().select(queryColumn).where(condition).fetchAll(clazz);
  }

  /**
   * 获取某页列表数据.
   *
   * @param <R> the type parameter
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param clazz the clazz
   * @param sortColumn the sort column
   * @return list list
   */
  default <R> List<R> lists(
      Consumer<Condition<T>> consumer,
      final int page,
      final int size,
      SortColumn sortColumn,
      @Nonnull Class<R> clazz) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return Collections.emptyList();
    }
    final QueryColumn<T> queryColumn = getQueryColumn();
    final Condition<T> condition = queryColumn.getCondition();
    consumer.accept(condition);
    return getQuery()
        .select(queryColumn)
        .where(queryColumn.getCondition())
        .fetchAll(clazz, pageable);
  }

  /**
   * Page page.
   *
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumn the sort column
   * @return the page
   * @see #page(Consumer, int, int, SortColumn, Class)
   */
  default Page<T> page(
      Consumer<QueryColumn<T>> consumer, final int page, final int size, SortColumn sortColumn) {
    return page(consumer, page, size, sortColumn, getPoJoClass(this));
  }

  /**
   * Page page.
   *
   * @param condition the condition
   * @param page the page
   * @param size the size
   * @param sortColumn the sort column
   * @return the page
   */
  default Page<T> pages(
      Consumer<ICondition<T>> condition, final int page, final int size, SortColumn sortColumn) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return new PageImpl<>(Collections.emptyList());
    }
    final QueryColumn<T> queryColumn = getQueryColumn();
    condition.accept(queryColumn.getCondition());
    return getQuery()
        .select(queryColumn)
        .where(queryColumn.getCondition())
        .fetchPage(getPoJoClass(this), pageable);
  }

  /**
   * Page page.
   *
   * @param <R> the type parameter
   * @param query the query
   * @param condition the condition
   * @param page the page
   * @param size the size
   * @param clazz the clazz
   * @param sortColumn the sort column
   * @return the page
   */
  default <R> Page<R> page(
      Consumer<QueryColumn<T>> query,
      Consumer<ICondition<T>> condition,
      final int page,
      final int size,
      final Class<R> clazz,
      SortColumn sortColumn) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return new PageImpl<>(Collections.emptyList());
    }
    final QueryColumn<T> queryColumn = getQueryColumn();
    query.accept(queryColumn);
    final Condition<T> cond = queryColumn.getCondition();
    Optional.ofNullable(condition).ifPresent(consumer -> consumer.accept(cond));
    return getQuery()
        .select(queryColumn)
        .where(queryColumn.getCondition())
        .fetchPage(clazz, pageable);
  }

  /**
   * 分页查询PoJo.
   *
   * @param query 查询字段
   * @param condition 查询条件
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumn 排序规则,null时按update_time倒序
   * @return the page
   */
  default Page<T> page(
      @Nonnull Consumer<QueryColumn<T>> query,
      @Nonnull Consumer<Condition<T>> condition,
      final int page,
      final int size,
      @Nonnull SortColumn sortColumn) {
    final QueryColumn<T> queryColumn = getQueryColumn();
    query.accept(queryColumn);
    condition.accept(queryColumn.getCondition());
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return new PageImpl<>(Collections.emptyList());
    }
    return getQuery()
        .select(queryColumn)
        .where(queryColumn.getCondition())
        .fetchPage(getPoJoClass(this), pageable);
  }

  /**
   * Page page.
   *
   * @param <R> the type parameter
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param clazz the clazz
   * @param sortColumn the sort column
   * @return the page
   */
  default <R> Page<R> page(
      @Nonnull Consumer<QueryColumn<T>> consumer,
      final int page,
      final int size,
      SortColumn sortColumn,
      @Nonnull Class<R> clazz) {
    final PageRequest pageable = pageRequest(page, size, sortColumn);
    if (Objects.isNull(pageable)) {
      return new PageImpl<>(Collections.emptyList());
    }
    final QueryColumn<T> queryColumn = getQueryColumn();
    consumer.accept(queryColumn);
    return getQuery()
        .select(queryColumn)
        .where(queryColumn.getCondition())
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
  default PageRequest pageRequest(final int page, final int size, SortColumn sortColumn) {
    // 默认以update_time倒序
    return pageRequest(
        page,
        size,
        Objects.nonNull(sortColumn)
            ? sortColumn.getSort()
            : SortColumn.of().desc(AbstractEntityPoJo::getCreateTime).getSort());
  }

  /**
   * 获取分页对象,支持排序.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sort 排序规则
   * @return the page request
   */
  default PageRequest pageRequest(final int page, final int size, Sort sort) {
    if ((page < 1 || size < 0) && page != size) {
      return null;
    }
    return page == -1
        ? PageRequest.of(0, Integer.MAX_VALUE, sort)
        : PageRequest.of(page - 1, size > 0 ? size : Constant.DEFAULT_PAGE_SIZE, sort);
  }
}
