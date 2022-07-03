package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.helper.SqlHelper;
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
public class StringCnd<T> extends AbstractCnd<T, StringCnd<T>>
    implements IStringCondition<T, StringCnd<T>> {

  protected StringCnd() {
    super();
  }

  public StringCnd(final Class<T> clazz) {
    super(clazz);
  }

  public static <T> StringCnd<T> of(final Class<T> clazz) {
    return new StringCnd<>(clazz);
  }

  @Override
  public StringCnd<T> eq(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> ne(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> gt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.GREATER, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> ge(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.GE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> lt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LESS, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> le(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> like(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.LIKE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> notLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_LIKE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> between(
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
  public StringCnd<T> notBetween(
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
  public StringCnd<T> isNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCnd<T> isNotNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCnd<T> in(
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
  public StringCnd<T> notIn(
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
  public StringCnd<T> orEq(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orNe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.NOT_EQUAL, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orGt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.GREATER, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orGe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.GE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orLt(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LESS, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orLe(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.LIKE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orNotLike(
      final boolean condition, @Nonnull final String column, final Object value) {
    if (condition) {
      or(column, MatchPattern.NOT_LIKE, value);
    }
    return this;
  }

  @Override
  public StringCnd<T> orBetween(
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
  public StringCnd<T> orNotBetween(
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
  public StringCnd<T> orIsNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCnd<T> orIsNotNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(column)
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public StringCnd<T> orIn(
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
  public StringCnd<T> orNotIn(
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
  public StringCnd<T> and(@Nonnull final StringCndGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public StringCnd<T> or(@Nonnull final StringCndGroup<T> group) {
    if (!group.isEmpty()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
      valueTypes.addAll(group.getCondition().getValueTypes());
    }
    return this;
  }

  @Override
  public StringCnd<T> and(final String left, final String operator, final Object right) {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator : "")
            .concat(left)
            .concat(operator)
            .concat(toPreFormatSqlVal(right)));
    valueTypes.add(ValueType.of(right));
    return this;
  }

  @Override
  public StringCnd<T> or(final String left, final String operator, final Object right) {
    conditionSql.add(
        (conditionSql.size() > 0 ? OR.operator : "")
            .concat(left)
            .concat(operator)
            .concat(toPreFormatSqlVal(right)));
    valueTypes.add(ValueType.of(right));
    return this;
  }

  @Override
  public StringCnd<T> groupBy(@Nonnull final String column) {
    groupBySql.add(column);
    return this;
  }
}
