package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.wind.core.util.*;
import java.sql.PreparedStatement;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.*;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.jdbc.core.*;
import org.springframework.stereotype.Component;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.optimizeQueryString;
import static io.github.ramerf.wind.core.util.StringUtils.doIfNonEmpty;
import static java.util.stream.Collectors.toCollection;

/**
 * 构建 SQL.所有的条件字符串构造,需要改为对象<br>
 * 如: OrdrByClause... <br>
 * TODO: 查询缓存
 *
 * <p>下面是新的实现思路 <br>
 * select {@link SqlFunction} <br>
 * from table1 left join table2<br>
 * where {@link SqlFunction} <br>
 * {@link SqlFunction}
 *
 * <p>当{@link QueryAlia#sqlFunction}为null时,退化为普通条件,否则就是函数
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/28
 */
@Slf4j
@Component
@SuppressWarnings("all")
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class Query extends AbstractExecutor {
  /*
   TODO-TXF: 添加支持: 查询包含指定数据(可能是多个)的分页数据,并置于首位
   思路: 构造OrderByClause,使用sql语法:
   1. orderby id <> ?
   2. orderby id not in (?,?) 特性
   3. orderby case when id=? then min_number else max_number end
  */
  private List<QueryColumn<?>> queryColumns;
  private List<ICondition<?>> conditions;
  private String queryString;
  private String conditionString;
  private String countString;
  private String orderByString;
  /** 暂时用于where之后的函数(group by等). */
  private final StringBuilder afterWhereString = new StringBuilder();

  private static JdbcTemplate JDBC_TEMPLATE;

  /** Instantiates a new Query. */
  public Query() {
    Query.JDBC_TEMPLATE = AppContextInject.getBean(JdbcTemplate.class);
  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static Query getInstance() {
    final Query bean = AppContextInject.getBean(Query.class);
    log.debug("getInstance:[{}]", bean);
    return bean;
  }

  /**
   * 指定查询列.
   *
   * @param queryColumns the query columns
   * @return the query
   */
  public Query select(final QueryColumn<?>... queryColumns) {
    this.queryColumns = new LinkedList<>(Arrays.asList(queryColumns));
    this.queryString =
        this.queryColumns.stream()
            .map(QueryColumn::getString)
            .collect(Collectors.joining(Constant.SEMICOLON));
    return this;
  }

  /**
   * 该方法暂时不用.
   *
   * @param conditions the conditions
   * @return the query
   */
  public Query from(final ICondition<?>... conditions) {
    // TODO-WARN: from方法
    throw CommonException.of("方法未实现");
  }

  /**
   * 指定查询条件.
   *
   * @param conditions the conditions
   * @return the query
   */
  public Query where(final ICondition<?>... conditions) {
    this.conditions = new LinkedList<>(Arrays.asList(conditions));
    String conditionString =
        this.conditions.stream()
            .map(ICondition::getString)
            .collect(Collectors.joining(AND.operator()));

    if (conditionString.endsWith(AND.operator())) {
      conditionString =
          conditionString.substring(0, conditionString.length() - AND.operator().length());
    }
    this.conditionString =
        this.conditions.stream()
            .map(o -> o.getQueryEntityMetaData().getFromTable())
            .collect(Collectors.joining(SEMICOLON.operator()));
    // TODO-WARN 修复orderBy从这里下手
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
   * 指定查询条件.
   *
   * @param consumers the consumers
   * @return the query
   * @see Condition
   */
  public Query where(final Consumer<Condition<?>>... consumers) {
    this.conditions =
        consumers.length > 0
            ? IntStream.range(0, consumers.length)
                .mapToObj(
                    i -> {
                      Optional.ofNullable(consumers[i])
                          .ifPresent(
                              consumer -> consumer.accept(queryColumns.get(i).getCondition()));
                      return queryColumns.get(i).getCondition();
                    })
                .collect(toCollection(LinkedList::new))
            : new LinkedList<>();
    String conditionString =
        this.conditions.stream()
            .map(ICondition::getString)
            .collect(Collectors.joining(AND.operator()));

    if (conditionString.endsWith(AND.operator())) {
      conditionString =
          conditionString.substring(0, conditionString.length() - AND.operator().length());
    }
    this.conditionString =
        this.conditions.stream()
            .map(o -> o.getQueryEntityMetaData().getFromTable())
            .collect(Collectors.joining(SEMICOLON.operator()));
    if (StringUtils.nonEmpty(conditionString)) {
      this.conditionString = this.conditionString.concat(WHERE.operator()).concat(conditionString);
    }
    return this;
  }

  /**
   * Group by 语句.<br>
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
  public Query groupBy(@Nonnull final GroupByClause... groupByClauses) {
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
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));
    final String sql = "SELECT %s FROM %s";
    final String queryString =
        String.format(sql, optimizeQueryString(this.queryString, clazz), conditionString);
    final AtomicInteger startIndex = new AtomicInteger(0);
    final List<Map<String, Object>> result =
        JDBC_TEMPLATE.query(
            queryString,
            ps ->
                conditions.stream()
                    .flatMap(condition -> condition.getValues(startIndex).stream())
                    .forEach(o -> o.accept(ps)),
            new ColumnMapRowMapper());

    if (CollectionUtils.isEmpty(result)) {
      return null;
    }
    if (result.size() > 1) {
      throw CommonException.of(ResultCode.API_TOO_MANY_RESULTS);
    }
    ResultHandler<Map<String, Object>, R> resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz)
            : new BeanResultHandler<>(clazz, queryColumns);
    return resultHandler.handle(result.get(0));
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
    final String sql = "SELECT %s FROM %s";
    final String queryString =
        String.format(sql, optimizeQueryString(this.queryString, clazz), conditionString);
    if (log.isDebugEnabled()) {
      log.debug("fetch:[{}]", queryString);
    }
    final AtomicInteger startIndex = new AtomicInteger(0);
    final List<Map<String, Object>> list =
        JDBC_TEMPLATE.query(
            queryString,
            ps ->
                conditions.stream()
                    .flatMap(condition -> condition.getValues(startIndex).stream())
                    .forEach(o -> o.accept(ps)),
            new ColumnMapRowMapper());
    if (CollectionUtils.isEmpty(list)) {
      return Collections.emptyList();
    }
    ResultHandler<Map<String, Object>, R> resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz)
            : new BeanResultHandler<>(clazz, queryColumns);
    return resultHandler.handle(list);
  }

  /**
   * 分页查询列表数据.
   *
   * @param <R> the type parameter
   * @param clazz the clazz
   * @param pageable the pageable
   * @return the list
   */
  public <R> List<R> fetchAll(final Class<R> clazz, final PageRequest pageable) {
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));

    // TODO-WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        pageable.getSort().stream()
            .map(
                s ->
                    s.getProperty()
                        .concat(Constant.DEFAULT_SPLIT_SPACE)
                        .concat(s.getDirection().name()))
            .collect(Collectors.joining(Constant.SEMICOLON));
    if (StringUtils.nonEmpty(orderByString)) {
      conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    }
    if (log.isDebugEnabled()) {
      log.debug("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }

    final String sql = "SELECT %s FROM %s LIMIT %s OFFSET %s";
    final String queryString =
        String.format(
            sql,
            optimizeQueryString(this.queryString, clazz),
            conditionString,
            pageable.getPageSize(),
            pageable.getOffset());
    final AtomicInteger startIndex = new AtomicInteger(0);
    final List<Map<String, Object>> list =
        JDBC_TEMPLATE.query(
            queryString,
            ps ->
                conditions.stream()
                    .flatMap(condition -> condition.getValues(startIndex).stream())
                    .forEach(o -> o.accept(ps)),
            new ColumnMapRowMapper());
    if (log.isDebugEnabled()) {
      log.debug("fetch:[{}]", list);
    }
    ResultHandler<Map<String, Object>, R> resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz)
            : new BeanResultHandler<>(clazz, queryColumns);
    return resultHandler.handle(list);
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
  public <R> Page<R> fetchPage(final Class<R> clazz, final PageRequest pageable) {
    doIfNonEmpty(afterWhereString.toString(), str -> conditionString = conditionString.concat(str));

    // TODO-WARN 这个orderBy有问题,需要拼接表别名,目前单表不会报错
    // 解决思路: 定义排序的对象里面包含表别名,自定义分页对象
    orderByString =
        pageable.getSort().stream()
            .map(
                s ->
                    s.getProperty()
                        .concat(Constant.DEFAULT_SPLIT_SPACE)
                        .concat(s.getDirection().name()))
            .collect(Collectors.joining(Constant.SEMICOLON));
    if (StringUtils.nonEmpty(orderByString)) {
      conditionString = conditionString.concat(ORDER_BY.operator()).concat(orderByString);
    }
    if (log.isDebugEnabled()) {
      log.debug("fetch:[queryString:{},conditionString:{}]", queryString, conditionString);
    }

    final String sql = "SELECT %s FROM %s LIMIT %s OFFSET %s";
    final String queryString =
        String.format(
            sql,
            optimizeQueryString(this.queryString, clazz),
            conditionString,
            pageable.getPageSize(),
            pageable.getOffset());
    if (log.isDebugEnabled()) {
      log.debug("fetch:[{}]", queryString);
    }
    final long total = fetchCount();
    final AtomicInteger startIndex = new AtomicInteger(0);
    final List<Map<String, Object>> list =
        total < 1
            ? null
            : JDBC_TEMPLATE.query(
                queryString,
                ps ->
                    conditions.stream()
                        .flatMap(condition -> condition.getValues(startIndex).stream())
                        .forEach(o -> o.accept(ps)),
                new ColumnMapRowMapper());
    // 从0开始
    final int currentPage = pageable.getPageNumber();
    // 每页大小
    final int pageSize = pageable.getPageSize();
    if (CollectionUtils.isEmpty(list)) {
      return PageUtils.toPage(Collections.emptyList(), 0, currentPage, pageSize);
    }
    ResultHandler resultHandler =
        BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
            ? new PrimitiveResultHandler<>(clazz)
            : new BeanResultHandler<>(clazz, queryColumns);
    return PageUtils.toPage(resultHandler.handle(list), total, currentPage, pageSize);
  }

  /**
   * count查询.
   *
   * @return long long
   */
  public long fetchCount() {
    doIfNonEmpty(afterWhereString.toString(), str -> countString = countString.concat(str));
    final boolean nonEmpty = StringUtils.nonEmpty(countString);
    final String sql =
        nonEmpty && countString.contains(GROUP_BY.operator())
            ? "SELECT SUM(b.a) FROM (SELECT 1 a FROM %s) b"
            : "SELECT COUNT(1) FROM %s";
    final String queryString = String.format(sql, nonEmpty ? countString : "");

    List<Consumer<PreparedStatement>> list = new LinkedList<>();

    final AtomicInteger index = new AtomicInteger(1);
    final AtomicInteger startIndex = new AtomicInteger(0);
    return JDBC_TEMPLATE.query(
        queryString,
        ps ->
            conditions.stream()
                .flatMap(condition -> condition.getValues(startIndex).stream())
                .forEach(o -> o.accept(ps)),
        (ResultSetExtractor<Integer>)
            rs -> {
              while (rs.next()) {
                return rs.getInt(1);
              }
              return 0;
            });
  }

  /**
   * 自定义sql查询列表.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param sql the sql
   * @param poJoClazz the clazz
   * @param respClazz the clazz
   * @param args the args
   * @return the list
   */
  public <T extends AbstractEntityPoJo, R> List<R> fetchBySql(
      final String sql, final Class<T> poJoClazz, final Class<R> respClazz, final Object... args) {
    final List<Map<String, Object>> list = JDBC_TEMPLATE.queryForList(sql, args);
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
    return JDBC_TEMPLATE.queryForObject(sql, args, Long.class);
  }
}
