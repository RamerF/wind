package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.math.BigDecimal;
import java.util.Objects;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.AS;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.DOT;
import static java.util.stream.Collectors.joining;

/**
 * sql查询列定义.即 select 后跟的字段.<br>
 * 注意:当查询所有字段(未指定查询列)时,如果属性的下划线格式与对应数据库列不匹配,返回对象的该属性值将始终为零值.<br>
 * 可以指定查询列或者确保返回对象的属性下划线格式与数据库列对应,详情见<br>
 * wind-test: DemoProductPoJo#getColumn
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@Slf4j
@EqualsAndHashCode(callSuper = true)
@SuppressWarnings("UnusedReturnValue")
public class QueryColumn<T extends AbstractEntityPoJo> extends AbstractQueryEntity<T> {
  /** 预留嵌套语句. */
  //  private List<QueryColumn<T>> children = new ArrayList<>();

  private Condition<T> condition = null;

  private StringCondition<T> stringCondition = null;

  private QueryColumn(final EntityInfo entityInfo) {
    setEntityInfo(entityInfo);
  }

  /**
   * Of query column.
   *
   * @param <T> the type parameter
   * @param configuration the configuration
   * @return the query column
   */
  public static <T extends AbstractEntityPoJo> QueryColumn<T> of(WindConfiguration configuration) {
    return new QueryColumn<>(EntityInfo.of(configuration));
  }

  /**
   * 新增查询列.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> col(final IFunction<T, ?> function) {
    return col(function, null);
  }

  /**
   * 新增查询列.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> col(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, null);
  }

  /**
   * Count query column.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> count(final IFunction<T, ?> function) {
    return count(function, null);
  }

  /**
   * Count query column.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> count(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, SqlAggregateFunction.COUNT);
  }

  /**
   * Sum query column.<br>
   *
   * @param function the function
   * @return the query column
   * @see #sum(IFunction, String) #sum(IFunction, String)
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function) {
    return sum(function, null);
  }

  /**
   * Sum query column.<br>
   * 不清楚返回类型的情况下使用{@link BigDecimal}
   *
   * <pre>
   * <b>注意:该列的返回类型与数据库对应关系</b>
   * <b>java           jdbc</b>
   * BigDecimal     bigint/numeric/decimal
   * Double         double/float
   *
   * </pre>
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, SqlAggregateFunction.SUM);
  }

  /**
   * Max query column.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> max(final IFunction<T, ?> function) {
    return max(function, null);
  }

  /**
   * Max query column.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> max(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, SqlAggregateFunction.MAX);
  }

  /**
   * Min query column.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> min(final IFunction<T, ?> function) {
    return min(function, null);
  }

  /**
   * Min query column.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> min(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, SqlAggregateFunction.MIN);
  }

  /** 添加查询对象(列/聚合函数). */
  private QueryColumn<T> add(
      final IFunction<T, ?> function, final String alia, final SqlFunction sqlFunction) {
    getQueryEntityMetaData()
        .queryAlias
        .add(
            QueryAlia.of(
                function,
                alia,
                getQueryEntityMetaData().getTableName(),
                getQueryEntityMetaData().getTableAlia(),
                sqlFunction));
    return this;
  }

  @Override
  public String getString() {
    final QueryEntityMetaData<T> metaData = getQueryEntityMetaData();
    if (CollectionUtils.isEmpty(metaData.queryAlias)) {
      EntityHelper.getEntityInfo(metaData.clazz).getEntityColumns().forEach(this::add);
    }
    return metaData.queryAlias.stream().map(QueryColumn::toColumnWithAlia).collect(joining(","));
  }

  /** 添加查询对象(列/聚合函数). */
  public QueryColumn<T> add(final EntityColumn entityColumn) {
    getQueryEntityMetaData()
        .queryAlias
        .add(
            QueryAlia.of(
                entityColumn.getField().getName(), // constraint format
                entityColumn.getName(),
                getQueryEntityMetaData().getTableName(),
                getQueryEntityMetaData().getTableAlia()));
    return this;
  }

  /** 增加额外的表别名前缀 */
  private static String toColumnWithAlia(final QueryAlia queryAlia) {
    final String alia = queryAlia.getColumnAlia();
    final String name = queryAlia.getColumnName();
    final String tableAlias = queryAlia.getTableAlia();

    final SqlFunction sqlFunction = queryAlia.getSqlFunction();
    final String queryName =
        Objects.isNull(sqlFunction)
            ? tableAlias.concat(DOT.operator()).concat(name)
            : sqlFunction.string(tableAlias.concat(DOT.operator()).concat(name));
    return queryName.concat(AS.operator()).concat(alia);
  }

  /**
   * Gets condition.
   *
   * @return the condition
   */
  public Condition<T> getCondition() {
    if (Objects.isNull(condition)) {
      condition = Condition.of(this);
    }
    return condition;
  }

  /**
   * Gets condition.
   *
   * @return the condition
   */
  public StringCondition<T> getStrCondition() {
    if (Objects.isNull(stringCondition)) {
      stringCondition = StringCondition.of(this);
    }
    return stringCondition;
  }
}
