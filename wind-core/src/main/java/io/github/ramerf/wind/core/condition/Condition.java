package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.support.ChainLinkedList;
import io.github.ramerf.wind.core.support.ChainList;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import static io.github.ramerf.wind.core.condition.Condition.MatchPattern.*;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toSqlVal;
import static io.github.ramerf.wind.core.util.BeanUtils.methodToColumn;
import static io.github.ramerf.wind.core.util.StringUtils.camelToUnderline;

/**
 * 条件构造.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@Slf4j
@Component
@ToString
@SuppressWarnings("all")
public class Condition<T extends AbstractEntity> extends AbstractQueryEntity<T>
    implements ICondition<T> {
  /** where后的字符串,参数占位符为 ?. */
  private List<String> conditionSql = new LinkedList<>();
  /** 占位符对应的值. */
  private ChainList<Object> values = new ChainLinkedList<>();

  public static <T extends AbstractEntity> Condition<T> of(QueryColumn<T> queryColumn) {
    final Condition<T> condition = new Condition<>();
    condition.queryEntityMetaData = queryColumn.queryEntityMetaData;
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
              .concat(methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toSqlVal(value)));
      values.add(value);
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
              .concat(methodToColumn(field))
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toSqlVal(value)));
      values.add(value);
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
              .concat(methodToColumn(field))
              .concat(MatchPattern.GREATER.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public <V> Condition<T> ge(@Nonnull final IConsumer<T, V> field, final V value) {
    return ge(true, field, value);
  }

  @Override
  public <V> Condition<T> ge(
      final boolean condition, @Nonnull final IConsumer<T, ?> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.GE.operator)
              .concat(toSqlVal(value)));
      values.add(value);
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
              .concat(methodToColumn(field))
              .concat(MatchPattern.LESS.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public <V> Condition<T> le(@Nonnull final IConsumer<T, V> field, final V value) {
    return le(true, field, value);
  }

  @Override
  public <V> Condition<T> le(
      final boolean condition, @Nonnull final IConsumer<T, ?> field, final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.LE.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public <V> Condition<T> like(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return like(true, field, value);
  }

  @Override
  public <V> Condition<T> like(
      final boolean condition, @Nonnull final IConsumer<T, ?> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)).concat(PERCENT.operator));
    }
    return this;
  }

  @Override
  public <V> Condition<T> likeLeft(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return likeLeft(true, field, value);
  }

  @Override
  public <V> Condition<T> likeLeft(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)));
    }
    return this;
  }

  @Override
  public <V> Condition<T> likeRight(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return likeRight(true, field, value);
  }

  @Override
  public <V> Condition<T> likeRight(
      final boolean condition, @Nonnull final IConsumer<T, ?> field, @Nonnull final V value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(String.valueOf(value).concat(PERCENT.operator));
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
              .concat(methodToColumn(field))
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)).concat(PERCENT.operator));
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
              .concat(methodToColumn(field))
              .concat(
                  String.format(MatchPattern.BETWEEN.operator, toSqlVal(start), toSqlVal(end))));
      values.add(start).add(end);
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
              .concat(methodToColumn(field))
              .concat(String.format(NOT_BETWEEN.operator, toSqlVal(start), toSqlVal(end))));
      values.add(start).add(end);
    }
    return this;
  }

  @Override
  public Condition<T> isNull(@Nonnull final IConsumer<T, ?> field) {
    return isNull(true, field);
  }

  @Override
  public Condition<T> isNull(final boolean condition, @Nonnull final IConsumer<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public Condition<T> isNotNull(@Nonnull final IConsumer<T, ?> field) {
    return isNotNull(true, field);
  }

  @Override
  public Condition<T> isNotNull(final boolean condition, @Nonnull final IConsumer<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
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
              .concat(methodToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(v -> this.values.add(v));
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
              .concat(methodToColumn(field))
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(v -> this.values.add(v));
    }
    return this;
  }

  // 下面是老接口

  @Override
  public Condition<T> eq(@Nonnull final IFunction<T, ?> field, final Object value) {
    return eq(true, field, value);
  }

  @Override
  public Condition<T> eq(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  private Condition<T> eq(@Nonnull final String field, final Object value) {
    conditionSql.add(
        (conditionSql.size() > 0 ? AND.operator : "")
            .concat(queryEntityMetaData.getTableAlia())
            .concat(DOT.operator)
            .concat(camelToUnderline(field))
            .concat(MatchPattern.EQUAL.operator)
            .concat(toSqlVal(value)));
    values.add(value);
    return this;
  }

  /** 连表条件. */
  @Override
  public <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return eq(true, field, queryColumn, field2);
  }

  /** 连表条件. */
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
              .concat(methodToColumn(field))
              .concat(MatchPattern.EQUAL.operator)
              .concat(queryColumn.queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field2)));
    }
    return this;
  }

  @Override
  public Condition<T> ne(@Nonnull final IFunction<T, ?> field, final Object value) {
    return ne(true, field, value);
  }

  @Override
  public Condition<T> ne(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.NOT_EQUAL.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public Condition<T> gt(@Nonnull final IFunction<T, ?> field, final Object value) {
    return gt(true, field, value);
  }

  @Override
  public Condition<T> gt(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.GREATER.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public Condition<T> ge(@Nonnull final IFunction<T, ?> field, final Object value) {
    return ge(true, field, value);
  }

  @Override
  public Condition<T> ge(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.GE.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public Condition<T> lt(@Nonnull final IFunction<T, ?> field, final Object value) {
    return lt(true, field, value);
  }

  @Override
  public Condition<T> lt(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.LESS.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public Condition<T> le(@Nonnull final IFunction<T, ?> field, final Object value) {
    return le(true, field, value);
  }

  @Override
  public Condition<T> le(
      final boolean condition, @Nonnull final IFunction<T, ?> field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.LE.operator)
              .concat(toSqlVal(value)));
      values.add(value);
    }
    return this;
  }

  @Override
  public Condition<T> like(@Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    return like(true, field, value);
  }

  @Override
  public Condition<T> like(
      final boolean condition, @Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)).concat(PERCENT.operator));
    }
    return this;
  }

  @Override
  public Condition<T> likeLeft(@Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    return likeLeft(true, field, value);
  }

  @Override
  public Condition<T> likeLeft(
      final boolean condition, @Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)));
    }
    return this;
  }

  @Override
  public Condition<T> likeRight(@Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    return likeRight(true, field, value);
  }

  @Override
  public Condition<T> likeRight(
      final boolean condition, @Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(String.valueOf(value).concat(PERCENT.operator));
    }
    return this;
  }

  @Override
  public Condition<T> notLike(@Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    return notLike(true, field, value);
  }

  @Override
  public Condition<T> notLike(
      final boolean condition, @Nonnull final IFunction<T, ?> field, @Nonnull final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(NOT_LIKE_PLAIN.operator, QUESTION_MARK.operator)));
      values.add(PERCENT.operator.concat(String.valueOf(value)).concat(PERCENT.operator));
    }
    return this;
  }

  @Override
  public Condition<T> between(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    return between(true, field, start, end);
  }

  @Override
  public Condition<T> between(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(
                  String.format(MatchPattern.BETWEEN.operator, toSqlVal(start), toSqlVal(end))));
      values.add(start).add(end);
    }
    return this;
  }

  @Override
  public Condition<T> notBetween(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    return notBetween(true, field, start, end);
  }

  @Override
  public Condition<T> notBetween(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(String.format(NOT_BETWEEN.operator, toSqlVal(start), toSqlVal(end))));
      values.add(start).add(end);
    }
    return this;
  }

  @Override
  public Condition<T> isNull(@Nonnull final IFunction<T, ?> field) {
    return isNull(true, field);
  }

  @Override
  public Condition<T> isNull(final boolean condition, @Nonnull final IFunction<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.IS_NULL.operator));
    }
    return this;
  }

  @Override
  public Condition<T> isNotNull(@Nonnull final IFunction<T, ?> field) {
    return isNotNull(true, field);
  }

  @Override
  public Condition<T> isNotNull(final boolean condition, @Nonnull final IFunction<T, ?> field) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(MatchPattern.IS_NOT_NULL.operator));
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
        conditionSql.add(BRACKET_FORMAT.format(childConditionsSql));
      }
    }
    return this;
  }

  @Override
  public Condition<T> in(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Collection<? extends Serializable> values) {
    return in(true, field, values);
  }

  @Override
  public Condition<T> in(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Collection<? extends Serializable> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(v -> this.values.add(v));
    }
    return this;
  }

  @Override
  public Condition<T> notIn(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Collection<? extends Serializable> values) {
    return notIn(true, field, values);
  }

  @Override
  public Condition<T> notIn(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final Collection<? extends Serializable> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(queryEntityMetaData.getTableAlia())
              .concat(DOT.operator)
              .concat(methodToColumn(field))
              .concat(
                  String.format(
                      NOT_IN.operator,
                      values.stream()
                          .map(SqlHelper::toSqlVal)
                          .collect(Collectors.joining(SqlOperator.SEMICOLON.operator)))));
      values.forEach(v -> this.values.add(v));
    }
    return this;
  }

  @Override
  public Condition<T> and(@Nonnull Condition<T> children) {
    if (Objects.nonNull(children)) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(BRACKET_FORMAT.format(children.getString())));
    }
    return this;
  }

  @Override
  public Condition<T> or(@Nonnull Condition<T> children) {
    if (Objects.nonNull(children)) {
      conditionSql.add(
          (conditionSql.size() > 0 ? OR.operator : "")
              .concat(BRACKET_FORMAT.format(children.getString())));
    }
    return this;
  }

  @Override
  public String getString() {
    final Condition<AbstractEntityPoJo> condition = (Condition<AbstractEntityPoJo>) this;
    condition.eq(logicDeleteField, logicNotDelete);
    return String.join(Constant.DEFAULT_SPLIT_SPACE, conditionSql);
  }

  @Override
  public ChainList<Object> getValues() {
    return values;
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
