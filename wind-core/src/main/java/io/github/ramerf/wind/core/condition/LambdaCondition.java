package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import java.util.Collection;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.*;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * Lambda条件构造.
 *
 * @since 2019/12/26
 * @author ramer
 */
@Slf4j
@SuppressWarnings("UnusedReturnValue")
public class LambdaCondition<T> extends AbstractCondition<T, LambdaCondition<T>>
    implements ILambdaCondition<T, LambdaCondition<T>> {

  protected LambdaCondition() {
    super();
  }

  protected LambdaCondition(final Class<T> clazz) {
    super(clazz);
  }

  public static <T> LambdaCondition<T> of(final Class<T> clazz) {
    return new LambdaCondition<>(clazz);
  }

  @Override
  public <V> LambdaCondition<T> eq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> ne(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> gt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.GREATER.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> ge(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.GE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> lt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.LESS.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> le(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.LE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> like(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> notLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> between(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final V start,
      final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, setter));
      valueTypes.add(ValueType.of(end, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> notBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final V start,
      final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      NOT_BETWEEN.operator, toPreFormatSqlVal(start), toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, setter));
      valueTypes.add(ValueType.of(end, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> isNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> isNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> in(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, setter)));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> notIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, setter)));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orEq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orNe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orGt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.GREATER.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orGe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.GE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orLt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.LESS.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orLe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.LE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orNotLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final V start,
      final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, setter));
      valueTypes.add(ValueType.of(end, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orNotBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final V start,
      final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      NOT_BETWEEN.operator, toPreFormatSqlVal(start), toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start, setter));
      valueTypes.add(ValueType.of(end, setter));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orIsNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orIsNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, setter)));
    }
    return this;
  }

  @Override
  public <V> LambdaCondition<T> orNotIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, setter)));
    }
    return this;
  }

  @Override
  public LambdaCondition<T> and(@Nonnull LambdaConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public LambdaCondition<T> or(@Nonnull LambdaConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public final LambdaCondition<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    groupBySql.add(getter.getColumn());
    return this;
  }
}
