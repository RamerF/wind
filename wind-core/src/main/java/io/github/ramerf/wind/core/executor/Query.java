package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.domain.*;
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
public class Query<T> implements Dao {
  private final Executor executor;
  // TODO WARN 如果要支持多数据源这里要改下
  private final Configuration configuration;
  private final Class<T> clazz;

  public Query(final Configuration configuration, final Class<T> clazz) {
    this.configuration = configuration;
    this.clazz = clazz;
    EntityHelper.getEntityInfo(clazz);
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    this.executor =
        new SimpleJdbcExecutor(
            configuration,
            jdbcEnvironment
                .getTransactionFactory()
                .newTransaction(jdbcEnvironment.getDataSource()));
  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  @SuppressWarnings("unchecked")
  public static <T> Query<T> getInstance(
      @Nonnull final Configuration configuration, @Nonnull final Class<T> clazz) {
    final Query<T> query = new Query<>(configuration, clazz);
    return (Query<T>)
        configuration
            .getInterceptorChain()
            .pluginAll(query, clazz, new Object[] {configuration, clazz});
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

  /** 查询单条记录. */
  public T fetchOne(@Nonnull final Condition<?, ?> condition) {
    return fetchOne(condition, null);
  }

  /** 查询单条记录. */
  public T fetchOne(@Nonnull final Condition<?, ?> condition, final Fields<T> fields) {
    return fetchOne(condition, null, clazz);
  }

  /** 查询单条记录. */
  public <R> R fetchOne(
      @Nonnull final Condition<?, ?> condition, final Fields<T> fields, final Class<R> clazz) {
    return fetchOne(condition, null, clazz, null);
  }

  /** 查询单条记录. */
  public <R> R fetchOne(
      @Nonnull final Condition<?, ?> condition,
      final Fields<T> fields,
      final Class<R> clazz,
      final ResultHandler<R> resultHandler) {
    final String orderByClause = getOrderByClause(null);
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(null);

    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields),
            getTableName(),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchOne:[{}]", sql);
    }
    return executor.fetchOne(
        new SqlParam<T>()
            .setSql(sql)
            .setClazz(clazz == null ? this.clazz : clazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        resultHandler);
  }

  /** 查询列表. */
  public List<T> fetchAll(@Nonnull final Condition<T, ?> condition) {
    return fetchAll(condition, null);
  }

  /** 查询列表. */
  public List<T> fetchAll(@Nonnull final Condition<T, ?> condition, final Fields<T> fields) {
    return fetchAll(condition, fields, null);
  }

  /** 查询列表,返回指定对象. */
  public <R> List<R> fetchAll(
      @Nonnull final Condition<T, ?> condition, final Fields<T> fields, final Class<R> clazz) {
    return fetchAll(condition, null, fields, clazz);
  }

  /** 查询列表,查询指定页,返回指定对象. */
  public <R> List<R> fetchAll(
      @Nonnull final Condition<T, ?> condition,
      final Pageable pageable,
      final Fields<T> fields,
      final Class<R> clazz) {
    final String orderByClause = getOrderByClause(pageable);
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(pageable);

    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields),
            getTableName(),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchAll:[{}]", sql);
    }
    return executor.fetchAll(
        new SqlParam<T>()
            .setSql(sql)
            .setClazz(clazz == null ? this.clazz : clazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        clazz);
  }

  /**
   * 分页查询.
   *
   * @param pageable 指定分页和排序.示例:
   *     <pre>
   *     <li><code>PageRequest.of(1);</code>
   *     <li><code>PageRequest.of(1, 10).desc(Foo::setId);</code>
   *     <li><code>cnds.limit(1);</code>
   *     <li><code>cnds.limit(1, 10).orderBy(Foo::setId);</code>
   *     <li><code>Pageable.unpaged();</code>
   *     </pre>
   */
  public <R> Page<R> fetchPage(
      final Condition<?, ?> condition,
      final Pageable pageable,
      final Fields<T> fields,
      final Class<R> clazz) {
    final String orderByClause = getOrderByClause(pageable);
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(pageable);

    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields),
            getTableName(),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchPage:[{}]", sql);
    }
    return this.executor.fetchPage(
        new SqlParam<T>()
            .setSql(sql)
            .setClazz(clazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        pageable,
        fetchCount(condition, this.clazz));
  }

  /** 获取查询列.{@code fields}为null时,根据配置判断是否写入null字段;当排除不为空时,忽略全局是否写入空配置 */
  private String getQueryString(final Fields<T> fields) {
    if (fields == null)
      return EntityUtils.getAllColumnFields(this.clazz, null).stream()
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
      final List<Field> allColumnFields = EntityUtils.getAllColumnFields(this.clazz, null);
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
    EntityInfo entityInfo = EntityHelper.getEntityInfo(this.clazz);
    return entityInfo.getName();
  }

  private String getConditionClause(Condition<?, ?> condition) {
    condition = condition.appendLogicNotDelete();
    return condition.isEmpty() ? "" : WHERE.operator().concat(condition.getString());
  }

  private String getOrderByClause(final Pageable pageRequest) {
    Pageable pageable = pageRequest == null ? Pageable.unpaged() : pageRequest;
    if (pageable == null) return "";
    String orderBy =
        pageable.getSort().stream()
            .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
            .collect(Collectors.joining(","));
    return orderBy.isEmpty() ? "" : " order by " + orderBy;
  }

  private String getLimitClause(final Pageable pageRequest) {
    Pageable pageable = pageRequest == null ? Pageable.unpaged() : pageRequest;
    return pageable.isPaged()
        ? String.format(" limit %s offset %s", pageable.getPageSize(), pageable.getOffset())
        : "";
  }
  /**
   * count查询.
   *
   * @return long long
   */
  public long fetchCount(@Nonnull final Condition<?, ?> condition, final Class<T> clazz) {
    final String conditionClause = getConditionClause(condition);
    final String sql =
        conditionClause.contains(GROUP_BY.operator())
            ? "select sum(b.a) from (select 1 a from %s%s) b"
            : "select count(1) from %s%s";
    return this.executor.fetchCount(
        new SqlParam<T>()
            .setSql(String.format(sql, getTableName(), conditionClause))
            .setClazz(Long.class)
            .setEntityClazz(clazz)
            .setAggregateFunction(AggregateSqlFunction.COUNT)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)));
  }

  @Override
  public Configuration getConfiguration() {
    return this.configuration;
  }
}
