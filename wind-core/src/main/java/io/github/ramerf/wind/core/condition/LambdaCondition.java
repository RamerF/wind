package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.*;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * Lambda条件构造.
 *
 * @since 2019/12/26
 * @author Tang Xiaofeng
 */
@Slf4j
@ToString
@SuppressWarnings("UnusedReturnValue")
public class LambdaCondition<T extends AbstractEntityPoJo<T, ?>> extends AbstractCondition<T> {

  protected LambdaCondition() {
    super();
  }

  protected LambdaCondition(final Class<T> clazz) {
    super(clazz);
  }

  protected LambdaCondition(final QueryColumn<T> queryColumn) {
    super(queryColumn);
  }

  protected LambdaCondition(final Class<T> clazz, final String tableName, final String tableAlia) {
    super(clazz, tableName, tableAlia);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaCondition<T> getInstance(
      final Class<T> clazz) {
    return new LambdaCondition<>(clazz);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaCondition<T> getInstance(
      final QueryColumn<T> queryColumn) {
    return new LambdaCondition<>(queryColumn);
  }

  public <V> LambdaCondition<T> eq(@Nonnull final IConsumer<T, V> field, final V value) {
    return eq(true, field, value);
  }

  public <V> LambdaCondition<T> eq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> ne(@Nonnull final IConsumer<T, V> field, final V value) {
    return ne(true, field, value);
  }

  public <V> LambdaCondition<T> ne(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> gt(@Nonnull final IConsumer<T, V> field, final V value) {
    return gt(true, field, value);
  }

  public <V> LambdaCondition<T> gt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GREATER.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> ge(@Nonnull final IConsumer<T, V> field, final V value) {
    return ge(true, field, value);
  }

  public <V> LambdaCondition<T> ge(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> lt(@Nonnull final IConsumer<T, V> field, final V value) {
    return lt(true, field, value);
  }

  public <V> LambdaCondition<T> lt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LESS.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> le(@Nonnull final IConsumer<T, V> field, final V value) {
    return le(true, field, value);
  }

  public <V> LambdaCondition<T> le(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> like(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return like(true, field, value);
  }

  public <V> LambdaCondition<T> like(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> notLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return notLike(true, field, value);
  }

  public <V> LambdaCondition<T> notLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> between(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return between(true, field, start, end);
  }

  public <V> LambdaCondition<T> between(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, field));
      valueTypes.add(ValueType.of(end, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> notBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return notBetween(true, field, start, end);
  }

  public <V> LambdaCondition<T> notBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      NOT_BETWEEN.operator, toPreFormatSqlVal(start), toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, field));
      valueTypes.add(ValueType.of(end, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> isNull(@Nonnull final IConsumer<T, V> field) {
    return isNull(true, field);
  }

  public <V> LambdaCondition<T> isNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  public <V> LambdaCondition<T> isNotNull(@Nonnull final IConsumer<T, V> field) {
    return isNotNull(true, field);
  }

  public <V> LambdaCondition<T> isNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  public <V> LambdaCondition<T> in(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return in(true, field, values);
  }

  public <V> LambdaCondition<T> in(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    return this;
  }

  public <V> LambdaCondition<T> notIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return notIn(true, field, values);
  }

  public <V> LambdaCondition<T> notIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> eq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final QueryColumn<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return eq(true, field, queryColumn, field2);
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> eq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(queryColumn.getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field2.getColumn()));
    }
    return this;
  }

  public LambdaCondition<T> exists(@Nonnull final Condition<T> childConditions) {
    return exists(true, childConditions);
  }

  public LambdaCondition<T> exists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    if (condition) {
      final String childConditionsSql = childConditions.getString();
      if (StringUtils.nonEmpty(childConditionsSql)) {
        final QueryEntityMetaData<T> entityMetaData = childConditions.getQueryEntityMetaData();
        String childQuery =
            (conditionSql.size() > 0 ? AND.operator : "")
                + SqlAggregateFunction.EXISTS.string(
                    "select 1 from ", entityMetaData.getFromTable(), " WHERE ", childConditionsSql);

        conditionSql.add(childQuery);
        valueTypes.addAll(((LambdaCondition<T>) childConditions).valueTypes);
      }
    }
    return this;
  }

  public <V> LambdaCondition<T> orEq(@Nonnull final IConsumer<T, V> field, final V value) {
    return orEq(true, field, value);
  }

  public <V> LambdaCondition<T> orEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orNe(@Nonnull final IConsumer<T, V> field, final V value) {
    return orNe(true, field, value);
  }

  public <V> LambdaCondition<T> orNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orGt(@Nonnull final IConsumer<T, V> field, final V value) {
    return orGt(true, field, value);
  }

  public <V> LambdaCondition<T> orGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GREATER.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orGe(@Nonnull final IConsumer<T, V> field, final V value) {
    return orGe(true, field, value);
  }

  public <V> LambdaCondition<T> orGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orLt(@Nonnull final IConsumer<T, V> field, final V value) {
    return orLt(true, field, value);
  }

  public <V> LambdaCondition<T> orLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LESS.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orLe(@Nonnull final IConsumer<T, V> field, final V value) {
    return orLe(true, field, value);
  }

  public <V> LambdaCondition<T> orLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return orLike(true, field, value);
  }

  public <V> LambdaCondition<T> orLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orNotLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return orNotLike(true, field, value);
  }

  public <V> LambdaCondition<T> orNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return orBetween(true, field, start, end);
  }

  public <V> LambdaCondition<T> orBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, field));
      valueTypes.add(ValueType.of(end, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orNotBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return orNotBetween(true, field, start, end);
  }

  public <V> LambdaCondition<T> orNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      NOT_BETWEEN.operator, toPreFormatSqlVal(start), toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, field));
      valueTypes.add(ValueType.of(end, field));
    }
    return this;
  }

  public <V> LambdaCondition<T> orIsNull(@Nonnull final IConsumer<T, V> field) {
    return orIsNull(true, field);
  }

  public <V> LambdaCondition<T> orIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  public <V> LambdaCondition<T> orIsNotNull(@Nonnull final IConsumer<T, V> field) {
    return orIsNotNull(true, field);
  }

  public <V> LambdaCondition<T> orIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  public <V> LambdaCondition<T> orIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return orIn(true, field, values);
  }

  public <V> LambdaCondition<T> orIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    return this;
  }

  public <V> LambdaCondition<T> orNotIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return orNotIn(true, field, values);
  }

  public <V> LambdaCondition<T> orNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> orEq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final QueryColumn<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return orEq(true, field, queryColumn, field2);
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> orEq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(queryColumn.getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(field2.getColumn()));
    }
    return this;
  }

  public LambdaCondition<T> orExists(@Nonnull final Condition<T> childConditions) {
    return orExists(true, childConditions);
  }

  public LambdaCondition<T> orExists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    if (condition) {
      final String childConditionsSql = childConditions.getString();
      if (StringUtils.nonEmpty(childConditionsSql)) {
        final QueryEntityMetaData<T> entityMetaData = childConditions.getQueryEntityMetaData();
        String childQuery =
            (conditionSql.size() > 0 ? OR.operator : "")
                + SqlAggregateFunction.EXISTS.string(
                    "select 1 from ", entityMetaData.getFromTable(), " WHERE ", childConditionsSql);
        conditionSql.add(childQuery);
        valueTypes.addAll(((LambdaCondition<T>) childConditions).valueTypes);
      }
    }
    return this;
  }

  public LambdaCondition<T> and(@Nonnull LambdaConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  public LambdaCondition<T> and(@Nonnull Consumer<LambdaConditionGroup<T>> group) {
    LambdaConditionGroup<T> conditionGroup = LambdaConditionGroup.getInstance(this);
    group.accept(conditionGroup);
    if (!conditionGroup.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(conditionGroup.getCondition().getString())));
      valueTypes.addAll(conditionGroup.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public LambdaCondition<T> and(final String sql) {
    super.and(sql);
    return this;
  }

  public LambdaCondition<T> or(@Nonnull LambdaConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  public LambdaCondition<T> or(@Nonnull Consumer<LambdaConditionGroup<T>> group) {
    LambdaConditionGroup<T> conditionGroup = LambdaConditionGroup.getInstance(this);
    group.accept(conditionGroup);
    if (!conditionGroup.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(conditionGroup.getCondition().getString())));
      valueTypes.addAll(conditionGroup.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public LambdaCondition<T> or(final String sql) {
    super.or(sql);
    return this;
  }
}
