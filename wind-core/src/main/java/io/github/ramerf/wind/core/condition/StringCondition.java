package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import java.util.Collection;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * 条件构造.
 *
 * @author ramer
 * @since 2019/12/26
 */
@Slf4j
public class StringCondition<T> extends AbstractCondition<T, StringCondition<T>>
    implements IStringCondition<T, StringCondition<T>> {

  protected StringCondition() {
    super();
  }

  public StringCondition(final Class<T> clazz) {
    super(clazz);
  }

  public static <T> StringCondition<T> of(final Class<T> clazz) {
    return new StringCondition<>(clazz);
  }

  @Override
  public StringCondition<T> eq(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> ne(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> gt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.GREATER, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> ge(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.GE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> lt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LESS, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> le(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> like(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LIKE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> notLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_LIKE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> between(
      final boolean condition, @Nonnull final String column, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start));
      valueTypes.add(ValueType.of(end));
    }
    return this;
  }

  @Override
  public StringCondition<T> notBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.NOT_BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start));
      valueTypes.add(ValueType.of(end));
    }
    return this;
  }

  @Override
  public StringCondition<T> isNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCondition<T> isNotNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCondition<T> in(
      final boolean condition, @Nonnull final String column, final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  @Override
  public StringCondition<T> notIn(
      final boolean condition, @Nonnull final String column, final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  @Override
  public StringCondition<T> orEq(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orNe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.NOT_EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orGt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.GREATER, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orGe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.GE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orLt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LESS, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orLe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LIKE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orNotLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.NOT_LIKE, value);
    }
    return this;
  }

  @Override
  public StringCondition<T> orBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start));
      valueTypes.add(ValueType.of(end));
    }
    return this;
  }

  @Override
  public StringCondition<T> orNotBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.NOT_BETWEEN.operator,
                      toPreFormatSqlVal(start),
                      toPreFormatSqlVal(end))));
      valueTypes.add(ValueType.of(start));
      valueTypes.add(ValueType.of(end));
    }
    return this;
  }

  @Override
  public StringCondition<T> orIsNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCondition<T> orIsNotNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCondition<T> orIn(
      final boolean condition, @Nonnull final String column, final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  @Override
  public StringCondition<T> orNotIn(
      final boolean condition, @Nonnull final String column, final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  @Override
  public StringCondition<T> and(@Nonnull final StringConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public StringCondition<T> or(@Nonnull final StringConditionGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public StringCondition<T> and(final String column, final String operator, final Object value) {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator : "")
            .concat(column)
            .concat(operator)
            .concat(toPreFormatSqlVal(value)));
    valueTypes.add(ValueType.of(value));
    return this;
  }

  @Override
  public StringCondition<T> or(final String column, final String operator, final Object value) {
    conditionSql.add(
        (conditionSql.size() > 0 ? OR.operator : "")
            .concat(column)
            .concat(operator)
            .concat(toPreFormatSqlVal(value)));
    valueTypes.add(ValueType.of(value));
    return this;
  }

  @Override
  public StringCondition<T> groupBy(@Nonnull final String column) {
    groupBySql.add(column);
    return this;
  }
}
