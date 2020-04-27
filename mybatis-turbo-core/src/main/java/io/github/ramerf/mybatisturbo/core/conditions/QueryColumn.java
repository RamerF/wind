package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.conditions.Predicate.SqlOperator;
import io.github.ramerf.mybatisturbo.core.config.MybatisTurboConfiguration;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.constant.Constant;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.factory.QueryColumnFactory;
import io.github.ramerf.mybatisturbo.core.handler.ResultHandler.QueryAlia;
import io.github.ramerf.mybatisturbo.core.util.CollectionUtils;
import java.util.*;
import java.util.stream.Collectors;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;

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
public class QueryColumn<T extends AbstractEntity> extends AbstractQueryEntity<T> {
  // 预留嵌套语句
  private List<QueryColumn<T>> children = new ArrayList<>();
  private Conditions<T> conditions = null;

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
  public static <T extends AbstractEntity> QueryColumn<T> of(
      MybatisTurboConfiguration configuration) {
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
    final QueryAlia columnAlia = QueryAlia.of(function, alia, queryEntityMetaData.getTableAlia());
    queryEntityMetaData.setTableName(columnAlia.getTableName());
    queryEntityMetaData.setFromTable(
        columnAlia
            .getTableName()
            .concat(Constant.DEFAULT_SPLIT_SPACE)
            .concat(columnAlia.getTableAlia()));
    queryEntityMetaData.columnAlias.add(columnAlia);
    return this;
  }

  @Override
  public String getString() {
    return CollectionUtils.isEmpty(queryEntityMetaData.columnAlias)
        ? queryEntityMetaData
            .getTableAlia()
            .concat(SqlOperator.DOT.operator())
            .concat(SqlOperator.WILDCARD.operator())
        : queryEntityMetaData.columnAlias.stream()
            .map(QueryColumn::methodToColumnWithAlia)
            .collect(Collectors.joining(Constant.DEFAULT_STRING_SPLIT));
  }

  /**
   * Gets conditions.
   *
   * @return the conditions
   */
  public Conditions<T> getConditions() {
    if (Objects.isNull(conditions)) {
      conditions = new Conditions<>();
      conditions.logicDeleted = this.logicDeleted;
      conditions.logicNotDelete = this.logicNotDelete;
      conditions.logicDeleteField = this.logicDeleteField;
      conditions.queryEntityMetaData = this.queryEntityMetaData;
    }
    return conditions;
  }

  /** 增加额外的表别名前缀 */
  private static String methodToColumnWithAlia(final QueryAlia columnAlia) {
    final String alia = columnAlia.getColumnAlia();
    final String name = columnAlia.getColumnName();
    final String tableAlias = columnAlia.getTableAlia();
    return tableAlias
        .concat(SqlOperator.DOT.operator())
        .concat(name)
        .concat(SqlOperator.AS.operator())
        .concat(alia);
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
