package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Pageable;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.handler.ResultHandlerUtil;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.GROUP_BY;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.WHERE;

/**
 * 通用查询操作对象.
 *
 * <p>获取PoJo的查询实例<br>
 *
 * <p>方式1<br>
 * {@code Query.getInstance(PoJo.class)}
 *
 * <p>方式2<br>
 * {@literal @Resource private Provider<Query<PoJo>> queryProvider;}
 *
 * <p>方式3<br>
 * {@literal @Resource private ObjectProvider<Query<PoJo>> queryProvider;}<br>
 * final Query<PoJo> query = queryProvider.get();
 *
 * <p>方式4<br>
 * {@literal @Resource private PrototypeBean prototypeBean;}<br>
 * final Query<PoJo> query = prototypeBean.query(PoJo.class);
 *
 * @author ramer
 * @since 2019/12/28
 */
@Slf4j
public class Query<T> {
  private Fields<T> fields;

  private Condition<?, ?> condition;
  private Pageable pageable = Pageable.unpaged();

  private final Executor executor;
  // TODO WARN 如果要支持多数据源这里要改下
  private static WindContext windContext;
  private static Configuration configuration;
  private final Class<T> clazz;

  public static void initial(final WindContext windContext) {
    Query.windContext = windContext;
    Query.configuration = windContext.getConfiguration();
  }

  public Query(final Class<T> clazz) {
    this.clazz = clazz;
    EntityHelper.getEntityInfo(clazz);
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    this.executor =
        configuration.newExecutor(
            jdbcEnvironment
                .getTransactionFactory()
                .newTransaction(jdbcEnvironment.getDataSource()));
  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static <T> Query<T> getInstance(final Class<T> clazz) {
    return new Query<>(clazz);
  }

  /** 指定查询列. */
  public final QueryWhere<T> select(final Fields<T> fields) {
    this.fields = fields;
    return new QueryWhere<>(this);
  }

  /** 自定义sql查询单个. */
  public <R> R fetchOneBySql(final String sql, final Class<R> respClazz, final Object... args) {
    List<R> rs = fetchListBySql(sql, respClazz, args);
    return rs.isEmpty() ? null : rs.get(0);
  }

  /** 自定义sql查询列表. */
  public <R> List<R> fetchListBySql(
      final String sql, final Class<R> respClazz, final Object... args) {
    return executor.query(
        new SqlParam<T>().setSql(sql).setClazz(respClazz),
        ps -> {
          for (int i = 1; i < args.length + 1; i++) {
            JdbcUtils.setObject(ps, i, args[i - 1]);
          }
        },
        ResultHandlerUtil.getResultHandler(respClazz));
  }

  /** 自定义sql查询count. */
  public long countBySql(final String sql, final Object... args) {
    return Optional.ofNullable(
            executor.<T, Long>queryForObject(
                new SqlParam<T>()
                    .setClazz(Long.class)
                    .setSql(sql)
                    .setAggregateFunction(AggregateSqlFunction.COUNT),
                args))
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
     * <li><code>PageRequest.of(1);</code>
     * <li><code>PageRequest.of(1, 10).desc(Foo::setId);</code>
     * <li><code>cnds.limit(1);</code>
     * <li><code>cnds.limit(1, 10).orderBy(Foo::setId);</code>
     * <li><code>Pageable.unpaged();</code>
     */
    public QueryExecutor<T> pageable(@Nonnull final Pageable pageRequest) {
      query.pageable = pageRequest;
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
    public <R> R fetchOne(final Class<R> clazz, final ResultHandler<R> resultHandler) {
      final String orderByClause = getOrderByClause();
      final String conditionClause = getConditionClause();
      final String limitClause = getLimitClause();

      final String templateSql = "select %s from %s%s";
      final String sql =
          String.format(
              templateSql,
              getQueryString(this.query.fields),
              getTableName(),
              conditionClause.concat(orderByClause).concat(limitClause));
      if (log.isTraceEnabled()) {
        log.debug("fetchOne:[{}]", sql);
      }
      return query.executor.fetchOne(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
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

      final String templateSql = "select %s from %s%s";
      final String sql =
          String.format(
              templateSql,
              getQueryString(this.query.fields),
              getTableName(),
              conditionClause.concat(orderByClause).concat(limitClause));
      if (log.isTraceEnabled()) {
        log.debug("fetchAll:[{}]", sql);
      }
      return query.executor.fetchAll(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
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

      final String templateSql = "select %s from %s%s";
      final String sql =
          String.format(
              templateSql,
              getQueryString(this.query.fields),
              getTableName(),
              conditionClause.concat(orderByClause).concat(limitClause));
      if (log.isTraceEnabled()) {
        log.debug("fetchPage:[{}]", sql);
      }
      return query.executor.fetchPage(
          new SqlParam<T>()
              .setSql(sql)
              .setClazz(clazz)
              .setCondition(query.condition)
              .setStartIndex(new AtomicInteger(1)),
          query.pageable,
          fetchCount(query.clazz));
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
              ? "select sum(b.a) from (select 1 a from %s%s) b"
              : "select count(1) from %s%s";
      return query.executor.fetchCount(
          new SqlParam<T>()
              .setSql(String.format(sql, getTableName(), conditionClause))
              .setClazz(Long.class)
              .setEntityClazz(clazz)
              .setAggregateFunction(AggregateSqlFunction.COUNT)
              .setCondition(query.condition)
              .setStartIndex(new AtomicInteger(1)));
    }

    /** 获取查询列.{@code fields}为null时,根据配置判断是否写入null字段;当排除不为空时,忽略全局是否写入空配置 */
    private String getQueryString(final Fields<T> fields) {
      if (fields == null)
        return EntityUtils.getAllColumnFields(this.query.clazz, null).stream()
            .map(EntityUtils::fieldToColumn)
            .collect(Collectors.joining(","));
      final Collection<Field> queryFields;
      final Set<Field> includeFields = fields.getIncludeFields();
      final Set<Field> excludeFields = fields.getExcludeFields();
      if (!includeFields.isEmpty()) {
        queryFields =
            excludeFields.isEmpty()
                ? includeFields
                : includeFields.stream()
                    .filter(include -> !excludeFields.contains(include))
                    .collect(Collectors.toSet());
      } else {
        final List<Field> allColumnFields = EntityUtils.getAllColumnFields(this.query.clazz, null);
        // 当排除不为空时,忽略是否写入空配置
        queryFields =
            excludeFields.isEmpty()
                ? allColumnFields
                : allColumnFields.stream()
                    .filter(include -> !excludeFields.contains(include))
                    .collect(Collectors.toSet());
      }
      return queryFields.stream().map(EntityUtils::fieldToColumn).collect(Collectors.joining(","));
    }

    private String getTableName() {
      EntityInfo entityInfo = EntityHelper.getEntityInfo(this.query.clazz);
      return entityInfo.getName();
    }

    private String getConditionClause() {
      return query.condition.isEmpty() ? "" : WHERE.operator().concat(query.condition.getString());
    }

    private String getOrderByClause() {
      if (query.pageable == null) return "";
      String orderBy =
          query.pageable.getSort().stream()
              .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
              .collect(Collectors.joining(","));
      return orderBy.isEmpty() ? "" : " order by " + orderBy;
    }

    private String getLimitClause() {
      return query.pageable.isPaged()
          ? String.format(
              " limit %s offset %s", query.pageable.getPageSize(), query.pageable.getOffset())
          : "";
    }
  }
}
