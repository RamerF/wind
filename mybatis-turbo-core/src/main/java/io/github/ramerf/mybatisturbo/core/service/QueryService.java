package io.github.ramerf.mybatisturbo.core.service;

import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.mybatisturbo.core.conditions.*;
import io.github.ramerf.mybatisturbo.core.conditions.Conditions.MatchPattern;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.constant.Constant;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.factory.QueryColumnFactory;
import io.github.ramerf.mybatisturbo.core.service.BaseService.ExtraProp;
import io.github.ramerf.mybatisturbo.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.Order;

import static io.github.ramerf.mybatisturbo.core.util.BeanUtils.getPoJoClass;
import static io.github.ramerf.mybatisturbo.core.util.BeanUtils.getResponseClass;

/**
 * 公共查询接口,<b>注意: 所有的方法忽略已删除记录.</b>
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings("all")
public interface QueryService<T extends AbstractEntityPoJo, E extends AbstractEntity>
    extends InterService<T, E> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(QueryService.class);

  /**
   * 条件is_delete=false的总记录数.
   *
   * @return long long
   */
  default long count() {
    return getRepository().selectCount(null);
  }

  /**
   * 条件is_delete=false的总记录数.
   *
   * @param consumer the consumer
   * @return long long
   */
  default long count(Consumer<QueryColumn<T>> consumer) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.<T>getInstance(getPoJoClass(this));
    consumer.accept(queryColumn);
    return Query.getInstance().select(queryColumn).where(queryColumn.getConditions()).fetchCount();
  }

  /**
   * 拼接条件is_delete=false.
   *
   * @param extraProps the extra props
   * @return the long
   */
  default long count(List<ExtraProp> extraProps) {
    return getRepository().selectCount(getWrapper(extraProps));
  }

  /**
   * 查询单个PoJo对象.
   *
   * @param extraProps 查询条件
   * @return the one
   * @see ExtraProp
   * @see ExtraProp#builder() ExtraProp#builder()
   */
  default T getOne(final List<ExtraProp> extraProps) {
    return getRepository().selectOne(getWrapper(extraProps));
  }

  /**
   * 由于众所周知的bug,该方法弃用,在下一个版本将会有不同的实现.<br>
   * 替换为: {@link #getOne(Consumer, Class)}
   *
   * @param <R> the type parameter
   * @param consumer the consumer
   * @return the one
   */
  @Deprecated
  default <R extends AbstractEntity> R getOne(Consumer<QueryColumn<T>> consumer) {
    return getOne(consumer, (Class<R>) getResponseClass(getClass()));
  }

  /**
   * 获取单个对象.
   *
   * @param query 指定查询字段
   * @param conditions 查询条件
   * @return {@link T}
   */
  default T getOne(Consumer<QueryColumn<T>> query, Consumer<Conditions<T>> conditions) {
    return getOne(query, conditions, getPoJoClass(this));
  }

  /**
   * 查询单个PoJo对象.
   *
   * @param consumer 查询条件
   * @return the ones
   */
  default T getOnes(Consumer<Conditions<T>> consumer) {
    return getOnes(consumer, getPoJoClass(this));
  }

  /**
   * 查询单个对象.
   *
   * @param <R> 返回对象
   * @param consumer 查询条件
   * @param clazz 返回对象
   * @return the ones
   */
  default <R> R getOnes(Consumer<Conditions<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    final Conditions<T> conditions = queryColumn.getConditions();
    consumer.accept(conditions);
    return Query.getInstance().select(queryColumn).where(conditions).fetchOne(clazz);
  }

  /**
   * 查询单个对象,可指定查询字段.
   *
   * @param <R> the type parameter
   * @param query 查询字段+条件
   * @param clazz 返回对象
   * @return the one
   * @see #getOne(Consumer, Consumer, Class) #getOne(Consumer, Consumer, Class)
   */
  default <R extends AbstractEntity> R getOne(Consumer<QueryColumn<T>> query, Class<R> clazz) {
    return getOne(query, null, clazz);
  }

  /**
   * 查询指定字段,返回单个对象.
   *
   * @param <R> 返回对象
   * @param query 查询字段
   * @param condition 查询条件
   * @param clazz 返回对象
   * @return the one
   */
  default <R> R getOne(
      Consumer<QueryColumn<T>> query, Consumer<Conditions<T>> condition, Class<R> clazz) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    query.accept(queryColumn);
    final Conditions<T> conditions = queryColumn.getConditions();
    Optional.ofNullable(condition).ifPresent(consumer -> consumer.accept(conditions));
    return Query.getInstance().select(queryColumn).where(conditions).fetchOne(clazz);
  }

  /**
   * 条件查询. 替代方法: {@link BaseService#list(Consumer)}
   *
   * @param extraProps 查询条件
   * @return {@link List <T>}
   */
  default List<T> list(List<ExtraProp> extraProps) {
    return Optional.ofNullable(getRepository().selectList(getWrapper(extraProps)))
        .orElse(Collections.emptyList());
  }

  /**
   * List list.
   *
   * @param extraProps the extra props
   * @param sortColumns the sort columns
   * @return the list
   */
  default List<T> list(List<ExtraProp> extraProps, SortColumn... sortColumns) {
    return page(-1, -1, extraProps, sortColumns).getRecords();
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
  default <R extends AbstractEntity> List<R> list(
      Consumer<QueryColumn<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    consumer.accept(queryColumn);
    return Query.getInstance().select(queryColumn).where(queryColumn.getConditions()).fetch(clazz);
  }

  /**
   * 查询指定字段,返回<code>PoJo</code>列表.
   *
   * @param <R> 返回对象
   * @param query 查询字段
   * @param condition 查询条件
   * @param clazz 返回对象
   * @return the list
   */
  default List<T> list(Consumer<QueryColumn<T>> query, Consumer<Conditions<T>> condition) {
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
      Consumer<QueryColumn<T>> query, Consumer<Conditions<T>> condition, final Class<R> clazz) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    query.accept(queryColumn);
    final Conditions<T> conditions = queryColumn.getConditions();
    Optional.ofNullable(condition).ifPresent(consumer -> consumer.accept(conditions));
    return Query.getInstance().select(queryColumn).where(queryColumn.getConditions()).fetch(clazz);
  }

  /**
   * 查询PoJo列表.
   *
   * @param consumer the consumer
   * @return the list
   */
  default List<T> lists(Consumer<Conditions<T>> consumer) {
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
  default <R extends AbstractEntity> List<R> lists(
      Consumer<Conditions<T>> consumer, Class<R> clazz) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.<T>getInstance(getPoJoClass(getClass()));
    final Conditions<T> conditions = queryColumn.getConditions();
    Optional.ofNullable(consumer).ifPresent(con -> con.accept(conditions));
    return Query.getInstance().select(queryColumn).where(conditions).fetch(clazz);
  }

  /**
   * 获取某页列表数据.
   *
   * @param <R> the type parameter
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param clazz the clazz
   * @param sortColumns the sort columns
   * @return list list
   */
  default <R extends AbstractEntity> List<R> lists(
      Consumer<Conditions<T>> consumer,
      final int page,
      final int size,
      @Nonnull Class<R> clazz,
      SortColumn... sortColumns) {
    final PageRequest pageable = pageRequest(page, size, Arrays.asList(sortColumns));
    if (Objects.isNull(pageable)) {
      return Collections.emptyList();
    }
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    final Conditions<T> conditions = queryColumn.getConditions();
    consumer.accept(conditions);
    return Query.getInstance()
        .select(queryColumn)
        .where(queryColumn.getConditions())
        .fetch(clazz, pageable);
  }

  /**
   * 通过id集合和企业id获取PoJo对象集合.
   *
   * @param ids the ids
   * @param companyId the companyId
   * @return the list
   */
  default List<T> listByIds(final Collection<? extends Serializable> ids, final long companyId) {
    return CollectionUtils.isEmpty(ids)
        ? Collections.emptyList()
        : lists(
            conditions ->
                conditions
                    .in(AbstractEntityPoJo::getId, ids)
                    .eq(AbstractEntityPoJo::getCompanyId, companyId));
  }

  /**
   * 不建议使用该方法.<br>
   * . 替代方法: {@link BaseService#page(int, int, List, SortColumn...)}}
   *
   * @param criteria 查询条件
   * @param page 当前页号 当page和size同时为-1时,将不会分页.
   * @param size 每页条目
   * @return {@link Page <T>}
   */
  @Deprecated
  default Page<T> page(final String criteria, final int page, final int size) {
    final PageRequest pageable = pageRequest(page, size);
    return Objects.isNull(pageable)
        ? new Page<>()
        : getRepository()
            .selectPage(
                new Page<>(pageable.getPageNumber(), pageable.getPageSize()), getWrapper(criteria));
  }

  /**
   * Page page.
   *
   * @param page the page
   * @param size the size
   * @param extraProps the extra props
   * @param sortColumns the sort columns
   * @return the page
   */
  default Page<T> page(
      final int page, final int size, List<ExtraProp> extraProps, SortColumn... sortColumns) {
    final PageRequest pageable = pageRequest(page, size);
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }
    final Page<T> queryPage = new Page<>(pageable.getPageNumber() + 1, pageable.getPageSize());
    queryPage.setOrders(
        Stream.of(sortColumns)
            .map(
                s -> {
                  OrderItem orderItem = new OrderItem();
                  orderItem.setAsc(s.isAsc());
                  orderItem.setColumn(s.getColumn());
                  return orderItem;
                })
            .collect(Collectors.toList()));
    return Optional.ofNullable(getRepository().selectPage(queryPage, getWrapper(extraProps)))
        .orElse(new Page<>());
  }

  /**
   * Page page.
   *
   * @param page the page
   * @param size the size
   * @param extraProps the extra props
   * @param sortColumns the sort columns
   * @return the page
   */
  default Page<T> page(
      final int page, final int size, List<ExtraProp> extraProps, List<SortColumn> sortColumns) {
    final PageRequest pageable = pageRequest(page, size);
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }
    final Page<T> queryPage = new Page<>(pageable.getPageNumber() + 1, pageable.getPageSize());
    queryPage.setOrders(
        sortColumns.stream()
            .map(
                s -> {
                  OrderItem orderItem = new OrderItem();
                  orderItem.setAsc(s.isAsc());
                  orderItem.setColumn(s.getColumn());
                  return orderItem;
                })
            .collect(Collectors.toList()));
    return Optional.ofNullable(getRepository().selectPage(queryPage, getWrapper(extraProps)))
        .orElse(new Page<>());
  }

  /**
   * Page page.
   *
   * @param page the page
   * @param size the size
   * @param companyId the company id
   * @param extraProps the extra props
   * @return the page
   */
  default Page<T> page(
      final int page, final int size, final long companyId, List<ExtraProp> extraProps) {
    final PageRequest pageable = pageRequest(page, size);
    return Objects.isNull(pageable)
        ? new Page<>()
        : Optional.ofNullable(
                getRepository()
                    .selectPage(
                        new Page<>(pageable.getPageNumber(), pageable.getPageSize()),
                        getWrapper(companyId, extraProps)))
            .orElse(new Page<>());
  }

  /**
   * Page page.
   *
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumns the sort columns
   * @return the page
   */
  default Page<T> page(
      Consumer<QueryColumn<T>> consumer,
      final int page,
      final int size,
      @Nonnull SortColumn... sortColumns) {
    return page(consumer, page, size, getPoJoClass(this), sortColumns);
  }

  /**
   * Page page.
   *
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumns the sort columns
   * @return the page
   */
  default Page<T> pages(
      Consumer<Condition<T>> condition,
      final int page,
      final int size,
      @Nonnull SortColumn... sortColumns) {
    final PageRequest pageable = pageRequest(page, size, Arrays.asList(sortColumns));
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    condition.accept(queryColumn.getConditions());
    return Query.getInstance()
        .select(queryColumn)
        .where(queryColumn.getConditions())
        .fetch(pageable, getPoJoClass(this));
  }

  /**
   * Page page.
   *
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumns the sort columns
   * @return the page
   */
  default <R> Page<R> page(
      Consumer<QueryColumn<T>> query,
      Consumer<Condition<T>> condition,
      final int page,
      final int size,
      final Class<R> clazz,
      @Nonnull SortColumn... sortColumns) {
    final PageRequest pageable = pageRequest(page, size, Arrays.asList(sortColumns));
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    query.accept(queryColumn);
    final Conditions<T> conditions = queryColumn.getConditions();
    Optional.ofNullable(condition).ifPresent(consumer -> consumer.accept(conditions));
    return Query.getInstance()
        .select(queryColumn)
        .where(queryColumn.getConditions())
        .fetch(pageable, clazz);
  }

  /**
   * 分页查询PoJo.
   *
   * @param query 查询字段
   * @param condition 查询条件
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumns 排序规则,参考: {@link SortColumn#desc(IFunction)}
   * @return the page
   */
  default Page<T> page(
      @Nonnull Consumer<QueryColumn<T>> query,
      @Nonnull Consumer<Conditions<T>> condition,
      final int page,
      final int size,
      @Nonnull SortColumn... sortColumns) {
    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    query.accept(queryColumn);
    condition.accept(queryColumn.getConditions());
    final PageRequest pageable = pageRequest(page, size, Arrays.asList(sortColumns));
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }
    return Query.getInstance()
        .select(queryColumn)
        .where(queryColumn.getConditions())
        .fetch(pageable, getPoJoClass(this));
  }

  /**
   * Page page.
   *
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumns the sort columns
   * @return the page
   */
  default Page<T> page(
      @Nonnull Consumer<QueryColumn<T>> consumer,
      final int page,
      final int size,
      @Nonnull List<SortColumn> sortColumns) {
    return page(consumer, page, size, getPoJoClass(this), sortColumns.toArray(new SortColumn[0]));
  }

  /**
   * Page page.
   *
   * @param <R> the type parameter
   * @param consumer the consumer
   * @param page the page
   * @param size the size
   * @param sortColumns the sort columns
   * @param clazz the clazz
   * @return the page
   */
  @SuppressWarnings("unchecked")
  default <R extends AbstractEntity> Page<R> page(
      @Nonnull Consumer<QueryColumn<T>> consumer,
      final int page,
      final int size,
      @Nonnull Class<R> clazz,
      @Nonnull SortColumn... sortColumns) {
    final PageRequest pageable = pageRequest(page, size, Arrays.asList(sortColumns));
    if (Objects.isNull(pageable)) {
      return new Page<>();
    }

    final QueryColumn<T> queryColumn = QueryColumnFactory.getInstance(getPoJoClass(this));
    consumer.accept(queryColumn);
    return Query.getInstance()
        .select(queryColumn)
        .where(queryColumn.getConditions())
        .fetch(pageable, clazz);
  }

  /**
   * 分页条件查询.
   *
   * @param companyId the company id
   * @param criteria 查询条件
   * @param page 当前页号 当page和size同时为-1时,将不会分页.
   * @param size 每页条目
   * @return {@link Page <T>}
   */
  @Deprecated
  default Page<T> page(
      final long companyId, final String criteria, final int page, final int size) {
    final PageRequest pageable = pageRequest(page, size);
    return Objects.isNull(pageable)
        ? new Page<>()
        : getRepository()
            .selectPage(
                new Page<>(pageable.getPageNumber(), pageable.getPageSize()),
                getWrapper(companyId, criteria));
  }

  // TODO-WARN: wrapper 批量更新
  // TODO-WARN: wrapper 批量删除

  /**
   * 获取模糊查询条件,子类根据需要覆写该方法.
   *
   * @param criteria the criteria
   * @return the wrapper
   */
  @Deprecated
  default Wrapper<T> getWrapper(String criteria) {
    return null;
  }

  /**
   * 获取模糊查询条件,子类应该根据需要覆写该方法.
   *
   * @param companyId the company id
   * @param criteria the criteria
   * @return the wrapper
   */
  @Deprecated
  default Wrapper<T> getWrapper(final long companyId, final String criteria) {

    return null;
  }

  /**
   * Gets wrapper.
   *
   * @param extraProps the extra props
   * @return the wrapper
   */
  default Wrapper<T> getWrapper(List<ExtraProp> extraProps) {
    if (CollectionUtils.isEmpty(extraProps)) {
      return null;
    }
    final QueryWrapper<T> query = Wrappers.query();
    extraProps.forEach(
        extraProp -> {
          final MatchPattern pattern = extraProp.getMatchPattern();
          final String name = extraProp.getName();
          final List<Object> value = extraProp.getValue();
          final boolean notEmpty = !CollectionUtils.isEmpty(value) && Objects.nonNull(value.get(0));
          switch (pattern) {
            case EQUAL:
              query.eq(notEmpty, name, value.get(0));
              break;
            case NOT_EQUAL:
              query.ne(notEmpty, name, value.get(0));
              break;
            case GREATER:
              query.gt(notEmpty, name, value.get(0));
              break;
            case GE:
              query.ge(notEmpty, name, value.get(0));
              break;
            case LESS:
              query.lt(notEmpty, name, value.get(0));
              break;
            case LE:
              query.le(notEmpty, name, value.get(0));
              break;
            case LIKE:
              query.like(notEmpty, name, notEmpty ? value.get(0) : "");
              break;
            case LIKE_LEFT:
              query.likeLeft(notEmpty, name, notEmpty ? value.get(0) : "");
              break;
            case LIKE_RIGHT:
              query.likeRight(notEmpty, name, notEmpty ? value.get(0) : "");
              break;
            case NOT_LIKE:
              query.notLike(notEmpty, name, notEmpty ? value.get(0) : "");
              break;
            case BETWEEN:
              query.between(notEmpty, name, value.get(0), value.get(1));
              break;
            case NOT_BETWEEN:
              query.notBetween(notEmpty, name, value.get(0), value.get(1));
              break;
            case IS_NULL:
              query.isNull(notEmpty, name);
              break;
            case IS_NOT_NULL:
              query.isNotNull(notEmpty, name);
              break;
            case EXISTS:
            case NOT_EXISTS:
              query.notExists(notEmpty, (String) value.get(0));
              break;
            case IN:
              query.in(notEmpty, name, value);
              break;
            case NOT_IN:
              query.notIn(notEmpty, name, value);
              break;
            default:
              throw CommonException.of(String.format("表达式[%s]不支持", pattern.name()));
          }
        });
    // 自动会加上这条语句
    // query.eq("is_delete", false);
    return query;
  }

  /**
   * Gets wrapper.
   *
   * @param companyId the company id
   * @param extraProps the extra props
   * @return the wrapper
   */
  default Wrapper<T> getWrapper(final long companyId, List<ExtraProp> extraProps) {
    if (companyId < 1) {
      throw CommonException.of("条件[company_id]不能为空");
    }
    extraProps.add(ExtraProp.of(AbstractEntityPoJo::getCompanyId, MatchPattern.EQUAL, companyId));
    return getWrapper(extraProps);
  }

  /**
   * 获取分页对象.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @return the page request
   */
  default PageRequest pageRequest(final int page, final int size) {
    return pageRequest(page, size, Sort.by(Direction.DESC, "updateTime"));
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

  /**
   * 获取分页对象,支持排序.
   *
   * @param page 当前页,从1开始
   * @param size 每页大小
   * @param sortColumns 排序规则
   * @return the page request
   */
  default PageRequest pageRequest(final int page, final int size, List<SortColumn> sortColumns) {
    if (CollectionUtils.isEmpty(sortColumns)) {
      sortColumns = null;
    }
    return pageRequest(
        page,
        size,
        Sort.by(
            Optional.ofNullable(sortColumns)
                .orElse(
                    Collections.singletonList(SortColumn.desc(AbstractEntityPoJo::getCreateTime)))
                .stream()
                .map(
                    sortColumn ->
                        sortColumn.isAsc()
                            ? Order.asc(sortColumn.getColumn())
                            : Order.desc(sortColumn.getColumn()))
                .collect(Collectors.toList())));
  }
}
