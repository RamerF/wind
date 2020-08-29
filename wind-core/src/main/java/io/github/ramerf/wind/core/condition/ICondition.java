package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.sql.PreparedStatement;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;

/**
 * The interface Condition.
 *
 * <p>TODO: 如果要实现完整的连表查询,考虑在该类添加join方法
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/6
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public interface ICondition<T extends AbstractEntity> extends Predicate<T> {

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @return the Condition
   * @see Condition#of(QueryColumn)
   */
  default Condition<T> condition() {
    return condition(false);
  }

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @param genAlia 是否生成新的表别名,用于子查询时传true
   * @return the Condition
   * @see Condition#of(QueryColumn)
   */
  Condition<T> condition(final boolean genAlia);

  /**
   * Eq condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> eq(final IConsumer<T, V> field, final V value);

  /**
   * Eq condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> eq(final boolean condition, final IConsumer<T, V> field, final V value);

  /**
   * Ne condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> ne(IConsumer<T, V> field, V value);

  /**
   * In condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param values the values
   * @return the condition
   */
  <V> Condition<T> in(IConsumer<T, V> field, Collection<V> values);

  /**
   * In condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param values the values
   * @return the condition
   */
  <V> Condition<T> in(boolean condition, IConsumer<T, V> field, Collection<V> values);

  /**
   * Not in condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param values the values
   * @return the condition
   */
  <V> Condition<T> notIn(IConsumer<T, V> field, Collection<V> values);

  /**
   * Not in condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param values the values
   * @return the condition
   */
  <V> Condition<T> notIn(boolean condition, IConsumer<T, V> field, Collection<V> values);

  /**
   * 连表条件.
   *
   * @param <R> the type parameter
   * @param <Q> the type parameter
   * @param field the field
   * @param queryColumn the query column
   * @param field2 the field 2
   * @return the condition
   */
  <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  /**
   * 连表条件.
   *
   * @param <R> the type parameter
   * @param <Q> the type parameter
   * @param condition the condition
   * @param field the field
   * @param queryColumn the query column
   * @param field2 the field 2
   * @return the condition
   */
  <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final boolean condition,
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  /**
   * Ne condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> ne(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Gt condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> gt(IConsumer<T, V> field, V value);

  /**
   * Gt condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> gt(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Ge condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> ge(IConsumer<T, V> field, V value);

  /**
   * Ge condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> ge(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Lt condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> lt(IConsumer<T, V> field, V value);

  /**
   * Lt condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> lt(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Le condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> le(IConsumer<T, V> field, V value);

  /**
   * Le condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> le(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Like condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> like(IConsumer<T, V> field, @Nonnull V value);

  /**
   * Like condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> like(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Not like condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> notLike(IConsumer<T, V> field, V value);

  /**
   * Not like condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> notLike(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Between condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  <V> Condition<T> between(IConsumer<T, V> field, V start, V end);

  /**
   * Between condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  <V> Condition<T> between(boolean condition, IConsumer<T, V> field, V start, V end);

  /**
   * Not between condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  <V> Condition<T> notBetween(IConsumer<T, V> field, V start, V end);

  /**
   * Not between condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  <V> Condition<T> notBetween(boolean condition, IConsumer<T, V> field, V start, V end);

  /**
   * Is null condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @return the condition
   */
  <V> Condition<T> isNull(IConsumer<T, V> field);

  /**
   * Is null condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  <V> Condition<T> isNull(boolean condition, IConsumer<T, V> field);

  /**
   * Is not null condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @return the condition
   */
  <V> Condition<T> isNotNull(IConsumer<T, V> field);

  /**
   * Is not null condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  <V> Condition<T> isNotNull(boolean condition, IConsumer<T, V> field);

  /**
   * Exists condition.
   *
   * @param childConditions the child conditions
   * @return the condition
   */
  Condition<T> exists(final Condition<T> childConditions);

  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动
  /**
   * Exists condition.
   *
   * @param condition the condition
   * @param childConditions the child conditions
   * @return the condition
   */
  Condition<T> exists(final boolean condition, final Condition<T> childConditions);

  /**
   * And condition.
   *
   * @param consumer the consumer
   * @return the condition
   */
  Condition<T> and(@Nonnull Consumer<Condition<T>> consumer);

  /**
   * And condition.
   *
   * @param condition the condition
   * @param consumer the consumer
   * @return the condition
   */
  Condition<T> and(final boolean condition, @Nonnull Consumer<Condition<T>> consumer);

  /**
   * And condition.
   *
   * @param children the children
   * @return the condition
   */
  Condition<T> and(Condition<T> children);

  /**
   * And condition.
   *
   * @param condition the condition
   * @param children the children
   * @return the condition
   */
  Condition<T> and(final boolean condition, @Nonnull final Condition<T> children);

  /**
   * Or condition.
   *
   * @param consumer the consumer
   * @return the condition
   */
  Condition<T> or(@Nonnull Consumer<Condition<T>> consumer);

  /**
   * Or condition.
   *
   * @param condition the condition
   * @param consumer the consumer
   * @return the condition
   */
  Condition<T> or(final boolean condition, @Nonnull Consumer<Condition<T>> consumer);

  /**
   * Or condition.
   *
   * @param children the children
   * @return the condition
   */
  Condition<T> or(Condition<T> children);

  /**
   * Or condition.
   *
   * @param condition the condition
   * @param children the children
   * @return the condition
   */
  Condition<T> or(final boolean condition, @Nonnull final Condition<T> children);

  /**
   * 获取占位符值.<br>
   *
   * @param startIndex 填充参数的起始索引,null时从0开始
   * @return 占位符对应的值 values
   */
  List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex);

  /**
   * 获取所有原始值.
   *
   * @return the value string
   */
  List<Object> getOriginValues();

  /**
   * 是否为空,true:不包含任何条件.
   *
   * @return the boolean
   */
  boolean isEmpty();
}
