package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.helper.TypeConverterHelper;
import io.github.ramerf.wind.core.helper.TypeConverterHelper.ValueType;
import io.github.ramerf.wind.core.util.StringUtils;
import java.sql.PreparedStatement;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.*;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;
import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;
import static java.util.stream.Collectors.toCollection;

/**
 * 条件构造.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@Slf4j
@Component
@ToString
public class Condition<T extends AbstractEntity> extends AbstractQueryEntity<T>
    implements ICondition<T> {
  /** where后的字符串,参数占位符为 ?. */
  private final List<String> conditionSql = new LinkedList<>();
  /** 占位符对应的值. */
  private final List<ValueType> valueTypes = new LinkedList<>();

  private boolean containLogicNotDelete = false;

  public static <T extends AbstractEntity> Condition<T> of(QueryColumn<T> queryColumn) {
    final Condition<T> condition = new Condition<>();
    condition.queryEntityMetaData = queryColumn.queryEntityMetaData;
    return condition;
  }

  @Override
  public Condition<T> condition() {
    final Condition<T> condition = new Condition<>();
    condition.queryEntityMetaData = this.queryEntityMetaData;
    return condition;
  }

  @Override
  public <V> Condition<T> eq(@Nonnull final IConsumer<T, V> field, final V value) {
    return eq(true, field, value);
  }

  @Override
  public <V> Condition<T> eq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> ne(@Nonnull final IConsumer<T, V> field, final V value) {
    return ne(true, field, value);
  }

  @Override
  public <V> Condition<T> ne(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> gt(@Nonnull final IConsumer<T, V> field, final V value) {
    return gt(true, field, value);
  }

  @Override
  public <V> Condition<T> gt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GREATER.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> ge(@Nonnull final IConsumer<T, V> field, final V value) {
    return ge(true, field, value);
  }

  @Override
  public <V> Condition<T> ge(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.GE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> lt(@Nonnull final IConsumer<T, V> field, final V value) {
    return lt(true, field, value);
  }

  @Override
  public <V> Condition<T> lt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LESS.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> le(@Nonnull final IConsumer<T, V> field, final V value) {
    return le(true, field, value);
  }

  @Override
  public <V> Condition<T> le(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.LE.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> like(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return like(true, field, value);
  }

  @Override
  public <V> Condition<T> like(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> notLike(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return notLike(true, field, value);
  }

  @Override
  public <V> Condition<T> notLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public <V> Condition<T> between(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return between(true, field, start, end);
  }

  @Override
  public <V> Condition<T> between(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
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

  @Override
  public <V> Condition<T> notBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return notBetween(true, field, start, end);
  }

  @Override
  public <V> Condition<T> notBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
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

  @Override
  public <V> Condition<T> isNull(@Nonnull final IConsumer<T, V> field) {
    return isNull(true, field);
  }

  @Override
  public <V> Condition<T> isNull(final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> Condition<T> isNotNull(@Nonnull final IConsumer<T, V> field) {
    return isNotNull(true, field);
  }

  @Override
  public <V> Condition<T> isNotNull(final boolean condition, @Nonnull final IConsumer<T, V> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.IS_NOT_NULL.operator));
    }
    return this;
  }

  @Override
  public <V> Condition<T> in(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return in(true, field, values);
  }

  @Override
  public <V> Condition<T> in(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
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

  @Override
  public <V> Condition<T> notIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return notIn(true, field, values);
  }

  @Override
  public <V> Condition<T> notIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
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

  @Override
  public <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return eq(true, field, queryColumn, field2);
  }

  @Override
  public <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(queryColumn.queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(field2.getColumn()));
    }
    return this;
  }

  @Override
  public Condition<T> exists(@Nonnull final Condition<T> childConditions) {
    return exists(true, childConditions);
  }

  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动

  @Override
  public Condition<T> exists(final boolean condition, @Nonnull final Condition<T> childConditions) {
    if (condition) {
      final String childConditionsSql = childConditions.getString();
      if (StringUtils.nonEmpty(childConditionsSql)) {
        conditionSql.add(PARENTHESIS_FORMAT.format(childConditionsSql));
      }
    }
    return this;
  }

  @Override
  public Condition<T> and(@Nonnull Condition<T> children) {
    return and(true, children);
  }

  @Override
  public Condition<T> and(final boolean condition, @Nonnull Condition<T> children) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(PARENTHESIS_FORMAT.format(children.getString())));
      valueTypes.addAll(children.valueTypes);
    }
    return this;
  }

  @Override
  public Condition<T> or(@Nonnull Condition<T> children) {
    return or(true, children);
  }

  @Override
  public Condition<T> or(final boolean condition, @Nonnull Condition<T> children) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(PARENTHESIS_FORMAT.format(children.getString())));
      valueTypes.addAll(children.valueTypes);
    }
    return this;
  }

  @Override
  public String getString() {
    if (!containLogicNotDelete) {
      appendLogicNotDelete();
      containLogicNotDelete = true;
    }
    return String.join(Constant.DEFAULT_SPLIT_SPACE, conditionSql);
  }

  private synchronized void appendLogicNotDelete() {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator : "")
            .concat(queryEntityMetaData.getTableAlia())
            .concat(DOT.operator)
            .concat(camelToUnderline(logicDeleteField))
            .concat(MatchPattern.EQUAL.operator)
            .concat(toPreFormatSqlVal(logicNotDelete)));
    final IConsumer<AbstractEntityPoJo, Boolean> beanFunction = AbstractEntityPoJo::setIsDelete;
    valueTypes.add(ValueType.of(logicNotDelete, beanFunction));
  }

  @Override
  public List<Function<PreparedStatement, Object>> getValues() {
    if (!containLogicNotDelete) {
      appendLogicNotDelete();
      containLogicNotDelete = true;
    }
    return valueTypes.stream()
        .map(
            valueType ->
                (Function<PreparedStatement, Object>)
                    ps -> TypeConverterHelper.toJdbcValue(valueType, ps))
        .collect(toCollection(LinkedList::new));
  }

  /** 属性匹配模式 */
  @SuppressWarnings("unused")
  public enum MatchPattern {
    /** = */
    EQUAL("="),
    /** != */
    NOT_EQUAL("!="),
    /** &gt; */
    GREATER(">"),
    /** &gt;= */
    GE(">="),
    /** &lt; */
    LESS("<"),
    /** &lt;= */
    LE("<="),
    LIKE_PLAIN(" LIKE %s "),
    /** LIKE %criteria% */
    LIKE(" LIKE '%%%s%%'"),
    /** LIKE %criteria */
    LIKE_LEFT(" LIKE '%%%s'"),
    /** LIKE criteria% */
    LIKE_RIGHT(" LIKE '%s%%'"),
    /** NOT LIKE %criteria% */
    NOT_LIKE_PLAIN(" NOT LIKE %s "),
    NOT_LIKE(" NOT LIKE '%%%s%%'"),
    /** BETWEEN start AND end */
    BETWEEN(" BETWEEN %s and %s"),
    /** NOT BETWEEN start AND end */
    NOT_BETWEEN(" NOT BETWEEN %s and %s"),
    /** IS NULL */
    IS_NULL(" IS NULL"),
    /** IS NOT NULL */
    IS_NOT_NULL(" IS NOT NULL"),
    /** EXISTS */
    EXISTS(" EXISTS(%s)"),
    /** NOT EXISTS */
    NOT_EXISTS(" NOT EXISTS(%s)"),
    /** IN () */
    IN(" IN (%s)"),
    /** NOT IN () */
    NOT_IN(" NOT IN (%s)"),
    /** &gt;ANY() */
    GREATER_ANY(" >ANY(%s)"),
    /** &gt;=ANY() */
    GE_ANY(" >=ANY(%s)"),
    /** &lt;ANY() */
    LESS_ANY(" <ANY(%s)"),
    /** &lt;=ANY() */
    LE_ANY(" <=ANY(%s)");

    private final String operator;

    MatchPattern(final String operator) {
      this.operator = operator;
    }
  }
}
