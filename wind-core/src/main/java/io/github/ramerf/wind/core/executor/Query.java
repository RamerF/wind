package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.condition.QueryColumn.QueryAlia;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.handler.ResultHandlerUtil;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.GROUP_BY;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.WHERE;
import static io.github.ramerf.wind.core.helper.SqlHelper.optimizeQueryString;

/**
 * 通用查询操作对象.
 *
 * <p>获取PoJo的查询实例<br>
 *
 * <p>方式1<br>
 * {@code Query.getInstance(PoJo.class)}
 *
 * <p>方式2<br>
 * {@code @Resource private Provider<Query<PoJo>> queryProvider;}
 *
 * <p>方式3<br>
 * {@code @Resource private ObjectProvider<Query<PoJo>> queryProvider;}<br>
 * final Query<PoJo> query = queryProvider.get();
 *
 * <p>方式4<br>
 * {@code @Resource private PrototypeBean prototypeBean;}<br>
 * final Query<PoJo> query = prototypeBean.query(PoJo.class);
 *
 * <p>构建 SQL.所有的条件字符串构造,需要改为对象<br>
 * 如: OrderByClause... <br>
 *
 * <p>下面是新的实现思路 <br>
 * select {@link SqlFunction} <br>
 * from table1 left join table2<br>
 * where {@link SqlFunction} <br>
 * {@link SqlFunction}
 *
 * <p>当{@link QueryAlia#getSqlFunction()}为null时,退化为普通条件,否则就是函数
 *
 * <p>TODO POST 完整的连表查询需要{@link Condition}支持
 *
 * @author ramer
 * @since 2019/12/28
 */
@Slf4j
public class Query<T> {
  private QueryColumn<T> queryColumn;

  private Condition<?, ?> condition;
  private Pageable pageable;

  private static Executor executor;
  private static WindConfiguration configuration;
  private static PrototypeBean prototypeBean;
  private final Class<T> clazz;

  public static void initial(
      final Executor executor,
      final WindConfiguration configuration,
      final PrototypeBean prototypeBean) {
    Query.executor = executor;
    Query.configuration = configuration;
    Query.prototypeBean = prototypeBean;
  }

  public Query(final Class<T> clazz) {
    this.clazz = clazz;
  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static <T> Query<T> getInstance(final Class<T> clazz) {
    return prototypeBean.query(clazz);
  }

  /**
   * 指定查询列.
   *
   * @param queryColumn the query column
   * @return the query
   */
  public final QueryWhere<T> select(@Nonnull final QueryColumn<T> queryColumn) {
    this.queryColumn = queryColumn;
    return new QueryWhere<>(this);
  }

  /** 自定义sql查询单个. */
  public <R> R fetchOneBySql(final String sql, final Class<R> respClazz, final Object... args) {
    final Map<String, Object> map =
        executor.queryForMap(new SqlParam<T>().setSql(sql).setClazz(respClazz), args);
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    return ResultHandlerUtil.handle(map, respClazz);
  }

  /** 自定义sql查询列表. */
  public <R> List<R> fetchListBySql(
      final String sql, final Class<R> respClazz, final Object... args) {
    final List<Map<String, Object>> list =
        executor.queryForList(new SqlParam<T>().setSql(sql).setClazz(respClazz), args);
    if (CollectionUtils.isEmpty(list)) {
      return Collections.emptyList();
    }
    return ResultHandlerUtil.handle(list, respClazz);
  }

  /** 自定义sql查询count. */
  public long countBySql(final String sql, final Object... args) {
    return Optional.ofNullable(
            executor.queryForObject(
                new SqlParam<T>().setSql(sql).setAggregateFunction(AggregateSqlFunction.COUNT),
                args,
                Long.class))
        .orElseGet(() -> (Long) BeanUtils.getPrimitiveDefaultValue(long.class));
  }

  public static class QueryWhere<T> {
    private final Query<T> query;

    QueryWhere(final Query<T> query) {
      this.query = query;
    }

    /** 指定查询条件. */
    public QueryExecutor<T> where(@Nonnull final Condition<?, ?> condition) {
      // 拼接逻辑删除
      query.condition = condition.appendLogicNotDelete();
      return new QueryExecutor<>(this.query);
    }
  }

  public static class QueryExecutor<T> {
    private final Query<T> query;

    QueryExecutor(final Query<T> query) {
      this.query = query;
    }

    /**
     * 指定分页和排序.示例:<br>
     * <li><code>Pages.of(1);</code><br>
     * <li><code>Pages.of(1, 10).desc(Foo::setId);</code><br>
     * <li><code>cnds.limit(1);</code>
     * <li><code>cnds.limit(1, 10).orderBy(Foo::setId);</code>
     */
    public QueryExecutor<T> pageable(final Pages pages) {
      query.pageable = pages == null ? null : pages.getPageable();
      return this;
    }

    /**
     * 查询单条记录.
     *
     * @param <R> the type parameter
     * @param clazz the clazz
     * @return the r
     */
    public <R> R fetchOne(final Class<R> clazz) {
      return fetchOne(clazz, null);
    }

    /**
     * 查询单条记录.
     *
     * @param <R> the type parameter
     * @param clazz the clazz
     * @return the r
     */
    public <R> R fetchOne(
        final Class<R> clazz, final ResultHandler<Map<String, Object>, R> resultHandler) {
      final String orderByClause = getOrderByClause();
      final String conditionClause = getConditionClause();
      final String limitClause = getLimitClause();

      final String templateSql = "select %s from %s";
      final String sql =
          String.format(
              templateSql,
              optimizeQueryString(query.queryColumn.getString(false), clazz),
              conditionClause.concat(orderByClause).concat(limitClause));
      return executor.fetchOne(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
              .setQueryColumn(query.queryColumn)
              .setStartIndex(new AtomicInteger(1)),
          resultHandler);
    }

    /**
     * 查询列表.
     *
     * @param <R> the type parameter
     * @param clazz the clazz
     * @return the list
     */
    public <R> List<R> fetchAll(final Class<R> clazz) {
      final String orderByClause = getOrderByClause();
      final String conditionClause = getConditionClause();
      final String limitClause = getLimitClause();

      final String templateSql = "select %s from %s";
      final String sql =
          String.format(
              templateSql,
              optimizeQueryString(query.queryColumn.getString(false), clazz),
              conditionClause.concat(orderByClause).concat(limitClause));
      if (log.isTraceEnabled()) {
        log.trace("fetchAll:sql:[{}]", sql);
      }
      return executor.fetchAll(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
              .setQueryColumn(query.queryColumn)
              .setStartIndex(new AtomicInteger(1)),
          clazz);
    }

    /**
     * 分页查询.
     *
     * @param <R> the type parameter
     * @param clazz the clazz
     * @return the page
     */
    public <R> Page<R> fetchPage(final Class<R> clazz) {
      final String orderByClause = getOrderByClause();
      final String conditionClause = getConditionClause();
      final String limitClause = getLimitClause();

      final String templateSql = "select %s from %s";
      final String sql =
          String.format(
              templateSql,
              optimizeQueryString(query.queryColumn.getString(false), clazz),
              conditionClause.concat(orderByClause).concat(limitClause));
      return executor.fetchPage(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
              .setQueryColumn(query.queryColumn)
              .setStartIndex(new AtomicInteger(1)),
          fetchCount(query.clazz),
          query.pageable);
    }

    /**
     * count查询.
     *
     * @return long long
     */
    public long fetchCount(final Class<T> clazz) {
      final String conditionClause = getConditionClause();
      final String sql =
          conditionClause.contains(GROUP_BY.operator())
              ? "select sum(b.a) from (select 1 a from %s) b"
              : "select count(1) from %s";
      return executor.fetchCount(
          new SqlParam<T>()
              .setSql(String.format(sql, conditionClause))
              .setClazz(Long.class)
              .setEntityClazz(clazz)
              .setAggregateFunction(AggregateSqlFunction.COUNT)
              .setCondition(query.condition)
              .setStartIndex(new AtomicInteger(1)));
    }

    private String getQueryClause() {
      return query.queryColumn.getString(false);
    }

    private String getFromTableClause() {
      return query.condition.getQueryEntityMetaData().getFromTable();
    }

    private String getConditionClause() {
      final String fromTables = query.condition.getQueryEntityMetaData().getFromTable();
      return query.condition.isEmpty()
          ? fromTables
          : fromTables.concat(WHERE.operator()).concat(query.condition.getString());
    }

    private String getOrderByClause() {
      return query.pageable == null
          ? ""
          : query.pageable.getSort().stream()
              .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
              .collect(Collectors.joining(","));
    }

    private String getLimitClause() {
      return query.pageable == null
          ? ""
          : String.format(
              " limit %s offset %s", query.pageable.getPageSize(), query.pageable.getOffset());
    }
  }
}
