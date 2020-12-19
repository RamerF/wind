package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import java.util.Collection;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
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

  public StringCondition<T> eq(@Nonnull final String column, @Nonnull final Object value) {
    return eq(true, column, value);
  }

  public StringCondition<T> eq(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.EQUAL, value);
    }
    return this;
  }

  public StringCondition<T> ne(@Nonnull final String column, @Nonnull final Object value) {
    return ne(true, column, value);
  }

  public StringCondition<T> ne(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_EQUAL, value);
    }
    return this;
  }

  public StringCondition<T> gt(@Nonnull final String column, @Nonnull final Object value) {
    return gt(true, column, value);
  }

  public StringCondition<T> gt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.GREATER, value);
    }
    return this;
  }

  public StringCondition<T> ge(@Nonnull final String column, @Nonnull final Object value) {
    return ge(true, column, value);
  }

  public StringCondition<T> ge(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.GE, value);
    }
    return this;
  }

  public StringCondition<T> lt(@Nonnull final String column, @Nonnull final Object value) {
    return lt(true, column, value);
  }

  public StringCondition<T> lt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.LESS, value);
    }
    return this;
  }

  public StringCondition<T> le(@Nonnull final String column, @Nonnull final Object value) {
    return le(true, column, value);
  }

  public StringCondition<T> le(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.LE, value);
    }
    return this;
  }

  public StringCondition<T> like(@Nonnull final String column, @Nonnull final Object value) {
    return like(true, column, value);
  }

  public StringCondition<T> like(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.LIKE, value);
    }
    return this;
  }

  public StringCondition<T> notLike(@Nonnull final String column, @Nonnull final Object value) {
    return notLike(true, column, value);
  }

  public StringCondition<T> notLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    if (condition) {
      and(column, MatchPattern.NOT_LIKE, value);
    }
    return this;
  }

  public StringCondition<T> between(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return between(true, column, start, end);
  }

  public StringCondition<T> between(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
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

  public StringCondition<T> notBetween(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return notBetween(true, column, start, end);
  }

  public StringCondition<T> notBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
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

  public StringCondition<T> isNull(@Nonnull final String column) {
    return isNull(true, column);
  }

  public StringCondition<T> isNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(column)
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  public StringCondition<T> isNotNull(@Nonnull final String column) {
    return isNotNull(true, column);
  }

  public StringCondition<T> isNotNull(final boolean condition, @Nonnull final String column) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(column)
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  public StringCondition<T> in(
      @Nonnull final String column, @Nonnull final Collection<? extends Iterable<?>> values) {
    return in(true, column, values);
  }

  public StringCondition<T> in(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Collection<? extends Iterable<?>> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  public StringCondition<T> notIn(
      @Nonnull final String column, @Nonnull final Collection<? extends Iterable<?>> values) {
    return notIn(true, column, values);
  }

  public StringCondition<T> notIn(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Collection<? extends Iterable<?>> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(column)
              .concat(
                  String.format(
                      MatchPattern.NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value)));
    }
    return this;
  }

  /** 用于关联对象. */
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

  protected StringCondition<T> and(
      final String column, final MatchPattern operator, final Object value) {
    return and(column, operator.operator, value);
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

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  public StringCondition<T> and(final String sql) {
    conditionSql.add((conditionSql.size() > 0 ? AND.operator : "").concat(sql));
    return this;
  }

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  public StringCondition<T> or(final String sql) {
    conditionSql.add((conditionSql.size() > 0 ? OR.operator : "").concat(sql));
    return this;
  }
}
