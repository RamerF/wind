package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.entity.constant.Constant.SEMICOLON;
import static io.github.ramerf.wind.core.helper.SqlHelper.toSqlVal;

/**
 * 条件构造.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@Slf4j
@Component
@SuppressWarnings("all")
public class Condition<T extends AbstractEntity> extends AbstractQueryEntity<T>
    implements ICondition<T> {
  private List<String> conditionSql = new LinkedList<>();

  public static <T extends AbstractEntity> Condition<T> of(QueryColumn<T> queryColumn) {
    final Condition<T> condition = new Condition<>();
    condition.queryEntityMetaData = queryColumn.queryEntityMetaData;
    return condition;
  }

  @Override
  public String getString() {
    final Condition<AbstractEntityPoJo> condition = (Condition<AbstractEntityPoJo>) this;
    condition.eq(logicDeleteField, logicNotDelete);
    return String.join(Constant.DEFAULT_SPLIT_SPACE, conditionSql);
  }

  @Override
  public <R> Condition<T> eq(final IConsumer<T, R> field, final R value) {
    return eq(true, field, value);
  }

  @Override
  public <R> Condition<T> eq(final boolean condition, final IConsumer<T, R> field, final R value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> eq(final IFunction<T, ?> field, final Object value) {
    return eq(true, field, value);
  }

  @Override
  public Condition<T> eq(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  private Condition<T> eq(final String field, final Object value) {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator() : "")
            .concat(queryEntityMetaData.getTableAlia())
            .concat(DOT.operator())
            .concat(StringUtils.camelToUnderline(field))
            .concat(MatchPattern.EQUAL.operator())
            .concat(toSqlVal(value)));
    return this;
  }

  /** 连表条件. */
  @Override
  public <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2) {
    return eq(true, field, queryColumn, field2);
  }

  /** 连表条件. */
  @Override
  public <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final boolean condition,
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator())
              .concat(queryColumn.queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field2)));
    }
    return this;
  }

  @Override
  public Condition<T> ne(final IFunction<T, ?> field, final Object value) {
    return ne(true, field, value);
  }

  @Override
  public Condition<T> ne(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.NOT_EQUAL.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> gt(final IFunction<T, ?> field, final Object value) {
    return gt(true, field, value);
  }

  @Override
  public Condition<T> gt(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.GREATER.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> ge(final IFunction<T, ?> field, final Object value) {
    return ge(true, field, value);
  }

  @Override
  public Condition<T> ge(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.GE.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> lt(final IFunction<T, ?> field, final Object value) {
    return lt(true, field, value);
  }

  @Override
  public Condition<T> lt(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.LESS.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> le(final IFunction<T, ?> field, final Object value) {
    return le(true, field, value);
  }

  @Override
  public Condition<T> le(final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.LE.operator())
              .concat(toSqlVal(value)));
    }
    return this;
  }

  @Override
  public Condition<T> like(final IFunction<T, ?> field, final Object value) {
    return like(true, field, value);
  }

  @Override
  public Condition<T> like(
      final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(String.format(MatchPattern.LIKE.operator(), value)));
    }
    return this;
  }

  @Override
  public Condition<T> likeLeft(final IFunction<T, ?> field, final Object value) {
    return likeLeft(true, field, value);
  }

  @Override
  public Condition<T> likeLeft(
      final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(String.format(MatchPattern.LIKE_LEFT.operator(), value)));
    }
    return this;
  }

  @Override
  public Condition<T> likeRight(final IFunction<T, ?> field, final Object value) {
    return likeRight(true, field, value);
  }

  @Override
  public Condition<T> likeRight(
      final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(String.format(MatchPattern.LIKE_RIGHT.operator(), value)));
    }
    return this;
  }

  @Override
  public Condition<T> notLike(final IFunction<T, ?> field, final Object value) {
    return notLike(true, field, value);
  }

  @Override
  public Condition<T> notLike(
      final boolean condition, final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(String.format(MatchPattern.NOT_LIKE.operator(), value)));
    }
    return this;
  }

  @Override
  public Condition<T> between(final IFunction<T, ?> field, final Object start, final Object end) {
    return between(true, field, start, end);
  }

  @Override
  public Condition<T> between(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(
                  String.format(MatchPattern.BETWEEN.operator(), toSqlVal(start), toSqlVal(end))));
    }
    return this;
  }

  @Override
  public Condition<T> notBetween(
      final IFunction<T, ?> field, final Object start, final Object end) {
    return notBetween(true, field, start, end);
  }

  @Override
  public Condition<T> notBetween(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.NOT_BETWEEN.operator(), toSqlVal(start), toSqlVal(end))));
    }
    return this;
  }

  @Override
  public Condition<T> isNull(final IFunction<T, ?> field) {
    return isNull(true, field);
  }

  @Override
  public Condition<T> isNull(final boolean condition, final IFunction<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.IS_NULL.operator()));
    }
    return this;
  }

  @Override
  public Condition<T> isNotNull(final IFunction<T, ?> field) {
    return isNotNull(true, field);
  }

  @Override
  public Condition<T> isNotNull(final boolean condition, final IFunction<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(MatchPattern.IS_NOT_NULL.operator()));
    }
    return this;
  }

  @Override
  public Condition<T> exists(final Condition<T> childConditions) {
    return exists(true, childConditions);
  }

  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动
  @Override
  public Condition<T> exists(final boolean condition, final Condition<T> childConditions) {
    if (condition) {
      final String childConditionsSql = childConditions.getString();
      if (StringUtils.nonEmpty(childConditionsSql)) {
        conditionSql.add(BRACKET_FORMAT.format(childConditionsSql));
      }
    }
    return this;
  }

  @Override
  public Condition<T> in(
      final IFunction<T, ?> field, final Collection<? extends Serializable> values) {
    return in(true, field, values);
  }

  @Override
  public Condition<T> in(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.IN.operator(),
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SEMICOLON)))));
    }
    return this;
  }

  @Override
  public Condition<T> notIn(
      final IFunction<T, ?> field, final Collection<? extends Serializable> values) {
    return notIn(true, field, values);
  }

  @Override
  public Condition<T> notIn(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator())
              .concat(BeanUtils.methodToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.NOT_IN.operator(),
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SEMICOLON)))));
    }
    return this;
  }

  @Override
  public Condition<T> and(Condition<T> children) {
    if (Objects.nonNull(children)) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(BRACKET_FORMAT.format(children.getString())));
    }
    return this;
  }

  @Override
  public Condition<T> or(Condition<T> children) {
    if (Objects.nonNull(children)) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator() : "")
              .concat(BRACKET_FORMAT.format(children.getString())));
    }
    return this;
  }

  /** 属性匹配模式 */
  public enum MatchPattern {
    /** = */
    EQUAL("="),
    /** != */
    NOT_EQUAL("!="),
    /** > */
    GREATER(">"),
    /** >= */
    GE(">="),
    /** < */
    LESS("<"),
    /** <= */
    LE("<="),
    /** LIKE %criteria% */
    LIKE(" LIKE '%%%s%%'"),
    /** LIKE %criteria */
    LIKE_LEFT(" LIKE '%%%s'"),
    /** LIKE criteria% */
    LIKE_RIGHT(" LIKE '%s%%'"),
    /** NOT LIKE %criteria% */
    NOT_LIKE(" NOT LIKE '%%%s%%'"),
    /** BETWEEN start AND end */
    BETWEEN(" BETWEEN %s and %s"),
    /** NOT BETWEEN start AND end */
    NOT_BETWEEN("NOT BETWEEN %s and %s"),
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
    /** >ANY() */
    GREATER_ANY(" >ANY(%s)"),
    /** >=ANY() */
    GE_ANY(" >=ANY(%s)"),
    /** <ANY() */
    LESS_ANY(" <ANY(%s)"),
    /** <=ANY() */
    LE_ANY(" <=ANY(%s)");

    private String operator;

    MatchPattern(final String operator) {
      this.operator = operator;
    }

    public String operator() {
      return operator;
    }
  }
}
