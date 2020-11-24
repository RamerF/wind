package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.EQUAL;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.AND;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.DOT;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * 条件构造.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@Slf4j
@ToString
public class StringCondition<T extends AbstractEntityPoJo<T, ?>> extends AbstractCondition<T> {

  public StringCondition(final QueryColumn<T> queryColumn) {
    super(queryColumn);
  }

  public StringCondition(final Class<T> clazz, final String tableName, final String tableAlia) {
    super(clazz, tableName, tableAlia);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> StringCondition<T> getInstance(
      final QueryColumn<T> queryColumn) {
    return new StringCondition<>(queryColumn);
  }

  public StringCondition<T> eq(@Nonnull final MappingInfo mappingInfo, final Object value) {
    return eq(true, mappingInfo, value);
  }

  public StringCondition<T> eq(
      final boolean condition, @Nonnull final MappingInfo mappingInfo, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(mappingInfo.getReferenceColumn())
              .concat(EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, mappingInfo.getReferenceField()));
    }
    return this;
  }

  public StringCondition<T> and(final String column, final String operator, final Object value) {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator : "")
            .concat(getQueryEntityMetaData().getTableAlia())
            .concat(DOT.operator)
            .concat(column)
            .concat(operator)
            .concat(toPreFormatSqlVal(value)));
    valueTypes.add(ValueType.of(value));
    return this;
  }
}
