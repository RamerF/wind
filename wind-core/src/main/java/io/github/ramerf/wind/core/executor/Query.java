package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.wind.core.util.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.optimizeQueryString;
import static io.github.ramerf.wind.core.util.StringUtils.doIfNonEmpty;

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
public class Query<T > {
  /**
   *
   *
   * <pre>
   * TODO POST 添加支持: 查询包含指定数据(可能是多个)的分页数据,并置于首位
   * 思路: 构造OrderByClause,使用sql语法:
   * 1. orderBy id <> ?
   * 2. orderBy id not in (?,?) 特性
   * 3. orderBy case when id=? then min_number else max_number end
   * </pre>
   */
  private List<QueryColumn<T>> queryColumns;

  private List<Condition<?>> conditions;
  private String queryString;
  private String conditionString;
  private String countString;
  private String orderByString;
  /** 暂时用于where之后的函数(group by等). */
  private final StringBuilder afterWhereString = new StringBuilder();

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
  public static <T  > Query<T> getInstance(final Class<T> clazz) {
    return prototypeBean.query(clazz);
  }

  /**
   * 指定查询列.
   *
   * @param queryColumns the query columns
   * @return the query
   */
  @SafeVarargs
  public final Query<T> select(@Nonnull final QueryColumn<T>... queryColumns) {
    this.queryColumns = new LinkedList<>(Arrays.asList(queryColumns));
    this.queryString =
        this.queryColumns.stream()
            .map(queryColumn -> queryColumn.getString(false))
            .collect(Collectors.joining(","));
    return this;
  }

  /**
   * 指定查询条件.
   *
   * @param conditions the conditions
   * @return the query
   */
  @SuppressWarnings("DuplicatedCode")
  public Query<T> where(final Condition<?>... conditions) {
    // 拼接逻辑删除
    for (Condition<?> condition : conditions) {
      condition.appendLogicNotDelete();
    }
    this.conditions = new LinkedList<>(Arrays.asList(conditions));
    String conditionString =
        this.conditions.stream()
            .map(Condition::getString)
            .collect(Collectors.joining(AND.operator()));

    if (conditionString.endsWith(AND.operator())) {
      conditionString =
          conditionString.substring(0, conditionString.length() - AND.operator().length());
    }
    // 连表查询时,要是queryColumns和conditions都只指定了一个就洗白了
    this.conditionString =
        this.conditions.stream()
            .map(o -> o.getQueryEntityMetaData().getFromTable())
            .collect(Collectors.joining(SEMICOLON.operator()));
    // TODO WARN 修复orderBy从这里下手
    // 每个条件带表别名
    // 定义orderByString ,这里赋值

    if (StringUtils.nonEmpty(conditionString)) {
      this.conditionString = this.conditionString.concat(WHERE.operator()).concat(conditionString);
    }

    if (StringUtils.nonEmpty(this.conditionString)) {
      this.countString = this.conditionString;
    }
    return this;
  }

  /**
   * Group by 语句.<br>
   * TODO WARN 这个思路有问题,groupBy的实现应该是{@link Condition},只是最后取sql做聚合
   *
   * <pre>
   *   初始化:
   *   final QueryEntityMetaData&lt;DemoProductPoJo&gt; queryEntityMetaData =
   *         queryColumn.getQueryEntityMetaData();
   *   final GroupByClause&lt;DemoProductPoJo&gt; clause = queryEntityMetaData.getGroupByClause();
   *   使用:
   *   groupBy(clause.col(DemoProductPoJo::getName))
   * </pre>
   *
   * @param groupByClauses the group by clauses
   * @return the query
   */
  public Query<T> groupBy(@Nonnull final GroupByClause<?>... groupByClauses) {
    afterWhereString
        .append(GROUP_BY)
        .append(
            Stream.of(groupByClauses)
                .flatMap(groupByClause -> groupByClause.getCols().stream())
                .collect(Collectors.joining()));
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
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));
    final String sql = "select %s from %s";
    final String queryString =
        String.format(sql, optimizeQueryString(this.queryString, clazz), conditionString);
    return executor.fetchOne(
        new SqlParam<T>()
            .setSql(queryString)
            .setClazz(clazz)
            .setConditions(conditions)
            .setQueryColumns(queryColumns)
            .setStartIndex(new AtomicInteger(1)),
        resultHandler);
  }

  /**
   * 查询列表数据.
   *
   * @param <R> the type parameter
   * @param clazz the clazz
   * @return the list
   */
  public <R> List<R> fetchAll(final Class<R> clazz) {
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));
    final String sql = "select %s from %s";
    final String queryString =
        String.format(sql, optimizeQueryString(this.queryString, clazz), conditionString);
    if (log.isTraceEnabled()) {
      log.trace("fetch:[{}]", queryString);
    }
    return executor.fetchAll(
        new SqlParam<T>()
            .setSql(queryString)
            .setClazz(clazz)
            .setConditions(conditions)
            .setQueryColumns(queryColumns)
            .setStartIndex(new AtomicInteger(1)),
        clazz);
  }

  /**
   * 分页查询列表数据.
   *
   * @param <R> the type parameter
   * @param clazz the clazz
   * @param pageable the pageable
   * @return the list
   */
  @SuppressWarnings("DuplicatedCode")
  public <R> List<R> fetchAll(final Class<R> clazz, final PageRequest pageable) {
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));
    // TODO WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        pageable.getSort().stream()
            .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
            .collect(Collectors.joining(","));
    if (StringUtils.nonEmpty(orderByString)) {
      conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    }
    if (log.isTraceEnabled()) {
      log.trace("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }
    final String sql = "select %s from %s limit %s offset %s";
    final String queryString =
        String.format(
            sql,
            optimizeQueryString(this.queryString, clazz),
            conditionString,
            pageable.getPageSize(),
            pageable.getOffset());
    return executor.fetchAll(
        new SqlParam<T>()
            .setSql(queryString)
            .setClazz(clazz)
            .setConditions(conditions)
            .setQueryColumns(queryColumns)
            .setStartIndex(new AtomicInteger(1)));
  }

  /**
   * 分页查询分页数据.<br>
   * <b>注意: 该方法不支持多表查询.</b>
   *
   * @param <R> the type parameter
   * @param clazz the clazz
   * @param pageable the pageable
   * @return the page
   */
  @SuppressWarnings("DuplicatedCode")
  public <R> Page<R> fetchPage(final Class<R> clazz, final PageRequest pageable) {
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));

    // TODO WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        pageable.getSort().stream()
            .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
            .collect(Collectors.joining(","));
    if (StringUtils.nonEmpty(orderByString)) {
      conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    }
    if (log.isTraceEnabled()) {
      log.trace("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }

    final String sql = "select %s from %s limit %s offset %s";
    final String queryString =
        String.format(
            sql,
            optimizeQueryString(this.queryString, clazz),
            conditionString,
            pageable.getPageSize(),
            pageable.getOffset());
    if (log.isTraceEnabled()) {
      log.trace("fetch:[{}]", queryString);
    }
    return executor.fetchPage(
        new SqlParam<T>()
            .setSql(queryString)
            .setClazz(clazz)
            .setConditions(conditions)
            .setQueryColumns(queryColumns)
            .setStartIndex(new AtomicInteger(1)),
        fetchCount(this.clazz),
        pageable);
  }

  /**
   * count查询.
   *
   * @return long long
   */
  public long fetchCount(final Class<T> clazz) {
    doIfNonEmpty(afterWhereString.toString(), str -> countString = countString.concat(str));
    final boolean nonEmpty = StringUtils.nonEmpty(countString);
    final String sql =
        nonEmpty && countString.contains(GROUP_BY.operator())
            ? "select sum(b.a) from (select 1 a from %s) b"
            : "select count(1) from %s";
    return executor.fetchCount(
        new SqlParam<T>()
            .setSql(String.format(sql, countString))
            .setClazz(Long.class)
            .setEntityClazz(clazz)
            .setAggregateFunction(SqlAggregateFunction.COUNT)
            .setConditions(conditions)
            .setStartIndex(new AtomicInteger(1)));
  }

  /**
   * 自定义sql查询单个.
   *
   * @param <R> the type parameter
   * @param sql the sql
   * @param respClazz the clazz
   * @param args the args
   * @return the list
   */
  public <R> R fetchOneBySql(final String sql, final Class<R> respClazz, final Object... args) {
    final Map<String, Object> map =
        executor.queryForMap(
            new SqlParam<T>().setSql(sql).setClazz(respClazz).setConditions(conditions), args);
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    // 用于转换列
    ResultHandler<Map<String, Object>, R> resultHandler =
        BeanUtils.isPrimitiveType(respClazz) || respClazz.isArray()
            ? new PrimitiveResultHandler<>(respClazz)
            : new BeanResultHandler<>(respClazz, queryColumns);
    return resultHandler.handle(map);
  }

  /**
   * 自定义sql查询列表.
   *
   * @param <R> the type parameter
   * @param sql the sql
   * @param respClazz the clazz
   * @param args the args
   * @return the list
   */
  public <R> List<R> fetchAllBySql(
      final String sql, final Class<R> respClazz, final Object... args) {
    final List<Map<String, Object>> list =
        executor.queryForList(
            new SqlParam<T>().setSql(sql).setClazz(respClazz).setConditions(conditions), args);
    if (CollectionUtils.isEmpty(list)) {
      return Collections.emptyList();
    }
    // 用于转换列
    ResultHandler<Map<String, Object>, R> resultHandler =
        BeanUtils.isPrimitiveType(respClazz) || respClazz.isArray()
            ? new PrimitiveResultHandler<>(respClazz)
            : new BeanResultHandler<>(respClazz, queryColumns);
    return resultHandler.handle(list);
  }

  /**
   * 自定义sql查询count.
   *
   * @param sql the sql
   * @param args the args
   * @return the list
   */
  public long countBySql(final String sql, final Object... args) {
    return executor.queryForObject(
        new SqlParam<T>()
            .setSql(sql)
            .setAggregateFunction(SqlAggregateFunction.COUNT)
            .setConditions(conditions),
        args,
        Long.class);
  }
}
