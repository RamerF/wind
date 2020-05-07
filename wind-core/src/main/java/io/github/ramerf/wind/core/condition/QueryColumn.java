package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.Objects;
import java.util.Optional;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.entity.constant.Constant.SEMICOLON;
import static java.util.stream.Collectors.joining;

/**
 * sql查询列定义.即 select 后跟的字段.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
@Slf4j
@Data
@EqualsAndHashCode(callSuper = true)
@SuppressWarnings("UnusedReturnValue")
public class QueryColumn<T extends AbstractEntity> extends AbstractQueryEntity<T> {
  /** 预留嵌套语句. */
  //  private List<QueryColumn<T>> children = new ArrayList<>();

  private Condition<T> condition = null;

  private QueryColumn() {}

  /**
   * Of query column.
   *
   * @param <T> the type parameter
   * @return the query column
   */
  public static <T extends AbstractEntity> QueryColumn<T> of() {
    return new QueryColumn<>();
  }

  /**
   * Of query column.
   *
   * @param <T> the type parameter
   * @param configuration the configuration
   * @return the query column
   */
  public static <T extends AbstractEntity> QueryColumn<T> of(WindConfiguration configuration) {
    final QueryColumn<T> queryColumn = new QueryColumn<>();
    queryColumn.logicDeleted = configuration.isLogicDeleted();
    queryColumn.logicNotDelete = configuration.isLogicNotDelete();
    queryColumn.logicDeleteField = configuration.getLogicDeleteField();
    return queryColumn;
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
   * Sum query column.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function) {
    return sum(function, null);
  }

  /**
   * Sum query column.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function, final String alia) {
    return add(function, alia, SqlAggregateFunction.SUM);
  }

  /** 添加查询对象(列/聚合函数). */
  private QueryColumn<T> add(
      final IFunction<T, ?> function, final String alia, final SqlFunction sqlFunction) {
    final QueryAlia queryAlia = QueryAlia.of(function, alia, queryEntityMetaData.getTableAlia());
    queryEntityMetaData.setTableName(queryAlia.getTableName());
    queryEntityMetaData.setFromTable(
        queryAlia
            .getTableName()
            .concat(Constant.DEFAULT_SPLIT_SPACE)
            .concat(queryAlia.getTableAlia()));
    Optional.ofNullable(sqlFunction).ifPresent(o -> queryAlia.setSqlFunction(sqlFunction));
    queryEntityMetaData.queryAlias.add(queryAlia);
    return this;
  }

  @Override
  public String getString() {
    return CollectionUtils.isEmpty(queryEntityMetaData.queryAlias)
        ? queryEntityMetaData.getTableAlia().concat(DOT.operator()).concat(WILDCARD.operator())
        : queryEntityMetaData.queryAlias.stream()
            .map(QueryColumn::methodToColumnWithAlia)
            .collect(joining(SEMICOLON));
  }

  /**
   * Gets condition.
   *
   * @return the condition
   */
  public Condition<T> getCondition() {
    if (Objects.isNull(condition)) {
      condition = new Condition<>();
      condition.logicDeleted = this.logicDeleted;
      condition.logicNotDelete = this.logicNotDelete;
      condition.logicDeleteField = this.logicDeleteField;
      condition.queryEntityMetaData = this.queryEntityMetaData;
    }
    return condition;
  }

  /** 增加额外的表别名前缀 */
  private static String methodToColumnWithAlia(final QueryAlia queryAlia) {
    final String alia = queryAlia.getColumnAlia();
    final String name = queryAlia.getColumnName();
    final String tableAlias = queryAlia.getTableAlia();

    // 待测试
    final SqlFunction sqlFunction = queryAlia.getSqlFunction();
    final String queryName =
        Objects.isNull(sqlFunction)
            ? tableAlias.concat(DOT.operator()).concat(name)
            : sqlFunction.string(tableAlias.concat(DOT.operator()).concat(name));
    // 待测试
    return queryName.concat(AS.operator()).concat(alia);
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    final QueryColumn<AbstractEntityPoJo> field =
        QueryColumnFactory.getInstance(AbstractEntityPoJo.class);
    System.out.println(field.col(AbstractEntityPoJo::getCompanyId).getString());
  }
}
