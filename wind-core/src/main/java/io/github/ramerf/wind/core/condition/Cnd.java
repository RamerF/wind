package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.helper.SqlHelper;
import java.util.Collection;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.*;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.*;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.ANY;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * Lambda条件构造.
 *
 * @since 2019/12/26
 * @author ramer
 */
@Slf4j
public class Cnd<T> extends AbstractCnd<T, Cnd<T>> implements ILambdaCondition<T, Cnd<T>> {

  protected Cnd() {
    super();
  }

  protected Cnd(final Class<T> clazz) {
    super(clazz);
  }

  public static <T> Cnd<T> of(final Class<T> clazz) {
    return new Cnd<>(clazz);
  }

  @Override
  public <V> Cnd<T> eq(
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
  public <V> Cnd<T> ne(
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
  public <V> Cnd<T> gt(
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
  public <V> Cnd<T> ge(
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
  public <V> Cnd<T> lt(
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
  public <V> Cnd<T> le(
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
  public <V> Cnd<T> like(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final String value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter, true));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> notLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final String value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, setter, true));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> between(
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
  public <V> Cnd<T> notBetween(
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
  public <V> Cnd<T> isNull(final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> isNotNull(final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> in(
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
  public <V> Cnd<T> notIn(
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
  public <V> Cnd<T> orEq(
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
  public <V> Cnd<T> orNe(
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
  public <V> Cnd<T> orGt(
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
  public <V> Cnd<T> orGe(
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
  public <V> Cnd<T> orLt(
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
  public <V> Cnd<T> orLe(
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
  public <V> Cnd<T> orLike(
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
  public <V> Cnd<T> orNotLike(
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
  public <V> Cnd<T> orBetween(
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
  public <V> Cnd<T> orNotBetween(
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
  public <V> Cnd<T> orIsNull(final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(setter.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> orIsNotNull(
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
  public <V> Cnd<T> orIn(
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
  public <V> Cnd<T> orNotIn(
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
  public Cnd<T> and(@Nonnull CndGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public Cnd<T> or(@Nonnull CndGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public final Cnd<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    groupBySql.add(getter.getColumn());
    return this;
  }

  /** {@inheritDoc} */
  @Override
  public <COLLECTION extends Collection<V>, V> Cnd<T> anyArray(
      boolean condition, @Nonnull SetterFunction<T, COLLECTION> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(toPreFormatSqlVal(value))
              .concat(MatchPattern.EQUAL.operator)
              .concat(ANY.operator + "(" + setter.getColumn() + ")"));
      valueTypes.add(ValueType.of(value, setter, true));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> contain(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(CONTAIN.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }

  @Override
  public <V> Cnd<T> contained(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(setter.getColumn())
              .concat(CONTAINED.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, setter));
    }
    return this;
  }
}
