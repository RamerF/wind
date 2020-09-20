package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.AbstractCondition.MatchPattern.EQUAL;
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
public class StringCondition<T extends AbstractEntity> extends AbstractCondition<T> {

  @Override
  public StringCondition<T> of() {
    return new StringCondition<>();
  }

  public static <T extends AbstractEntity> StringCondition<T> of(QueryColumn<T> queryColumn) {
    final StringCondition<T> condition = new StringCondition<>();
    condition.setEntityInfo(queryColumn.getEntityInfo());
    condition.setQueryEntityMetaData(queryColumn.getQueryEntityMetaData());
    return condition;
  }

  public <V> StringCondition<T> eq(@Nonnull final MappingInfo mappingInfo, final V value) {
    return eq(true, mappingInfo, value);
  }

  public <V> StringCondition<T> eq(
      final boolean condition, @Nonnull final MappingInfo mappingInfo, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(mappingInfo.getKey())
              .concat(EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, mappingInfo.getField()));
    }
    return this;
  }

  public <V> StringCondition<T> eq(@Nonnull final Field field, final V value) {
    return eq(true, field, value);
  }

  public <V> StringCondition<T> eq(
      final boolean condition, @Nonnull final Field field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(EntityUtils.fieldToColumn(field))
              .concat(EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }
}
