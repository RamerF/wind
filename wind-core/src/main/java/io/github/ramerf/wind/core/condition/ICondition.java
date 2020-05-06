package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.support.ChainList;
import java.io.Serializable;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * The interface Condition.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 20 -1-6
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public interface ICondition<T extends AbstractEntity> extends Predicate<T> {

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
   * Eq condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> eq(final IFunction<T, ?> field, final Object value);

  /**
   * Eq condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> eq(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * 连表条件. @param <R> the type parameter
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
   * 连表条件. @param <R> the type parameter
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
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> ne(final IFunction<T, ?> field, final Object value);

  /**
   * Ne condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> ne(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Gt condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> gt(final IFunction<T, ?> field, final Object value);

  /**
   * Gt condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> gt(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Ge condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> ge(final IFunction<T, ?> field, final Object value);

  /**
   * Ge condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> ge(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Lt condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> lt(final IFunction<T, ?> field, final Object value);

  /**
   * Lt condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> lt(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Le condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> le(final IFunction<T, ?> field, final Object value);

  /**
   * Le condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> le(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Like condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> like(final IFunction<T, ?> field, final Object value);

  /**
   * Like condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> like(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Like left condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> likeLeft(final IFunction<T, ?> field, final Object value);

  /**
   * Like left condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> likeLeft(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Like right condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> likeRight(final IFunction<T, ?> field, final Object value);

  /**
   * Like right condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> likeRight(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Not like condition.
   *
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> notLike(final IFunction<T, ?> field, final Object value);

  /**
   * Not like condition.
   *
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  Condition<T> notLike(final boolean condition, final IFunction<T, ?> field, final Object value);

  /**
   * Between condition.
   *
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  Condition<T> between(final IFunction<T, ?> field, final Object start, final Object end);

  /**
   * Between condition.
   *
   * @param condition the condition
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  Condition<T> between(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  /**
   * Not between condition.
   *
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  Condition<T> notBetween(final IFunction<T, ?> field, final Object start, final Object end);

  /**
   * Not between condition.
   *
   * @param condition the condition
   * @param field the field
   * @param start the start
   * @param end the end
   * @return the condition
   */
  Condition<T> notBetween(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  /**
   * Is null condition.
   *
   * @param field the field
   * @return the condition
   */
  Condition<T> isNull(final IFunction<T, ?> field);

  /**
   * Is null condition.
   *
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  Condition<T> isNull(final boolean condition, final IFunction<T, ?> field);

  /**
   * Is not null condition.
   *
   * @param field the field
   * @return the condition
   */
  Condition<T> isNotNull(final IFunction<T, ?> field);

  /**
   * Is not null condition.
   *
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  Condition<T> isNotNull(final boolean condition, final IFunction<T, ?> field);

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
  <V> Condition<T> ge(boolean condition, IConsumer<T, ?> field, V value);

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
  <V> Condition<T> le(boolean condition, IConsumer<T, ?> field, V value);

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
  <V> Condition<T> like(boolean condition, IConsumer<T, ?> field, V value);

  /**
   * Like left condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> likeLeft(IConsumer<T, V> field, V value);

  /**
   * Like left condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> likeLeft(boolean condition, IConsumer<T, V> field, V value);

  /**
   * Like right condition.
   *
   * @param <V> the type parameter
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> likeRight(IConsumer<T, V> field, V value);

  /**
   * Like right condition.
   *
   * @param <V> the type parameter
   * @param condition the condition
   * @param field the field
   * @param value the value
   * @return the condition
   */
  <V> Condition<T> likeRight(boolean condition, IConsumer<T, ?> field, V value);

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
   * @param field the field
   * @return the condition
   */
  Condition<T> isNull(IConsumer<T, ?> field);

  /**
   * Is null condition.
   *
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  Condition<T> isNull(boolean condition, IConsumer<T, ?> field);

  /**
   * Is not null condition.
   *
   * @param field the field
   * @return the condition
   */
  Condition<T> isNotNull(IConsumer<T, ?> field);

  /**
   * Is not null condition.
   *
   * @param condition the condition
   * @param field the field
   * @return the condition
   */
  Condition<T> isNotNull(boolean condition, IConsumer<T, ?> field);

  /**
   * Exists condition.
   *
   * @param childConditions the child conditions
   * @return the condition
   */
  Condition<T> exists(final Condition<T> childConditions);

  /**
   * Exists condition.
   *
   * @param condition the condition
   * @param childConditions the child conditions
   * @return the condition
   */
  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动
  Condition<T> exists(final boolean condition, final Condition<T> childConditions);

  /**
   * In condition.
   *
   * @param field the field
   * @param values the values
   * @return the condition
   */
  Condition<T> in(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  /**
   * In condition.
   *
   * @param condition the condition
   * @param field the field
   * @param values the values
   * @return the condition
   */
  Condition<T> in(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  /**
   * Not in condition.
   *
   * @param field the field
   * @param values the values
   * @return the condition
   */
  Condition<T> notIn(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  /**
   * Not in condition.
   *
   * @param condition the condition
   * @param field the field
   * @param values the values
   * @return the condition
   */
  Condition<T> notIn(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  /**
   * And condition.
   *
   * @param children the children
   * @return the condition
   */
  Condition<T> and(Condition<T> children);

  /**
   * Or condition.
   *
   * @param children the children
   * @return the condition
   */
  Condition<T> or(Condition<T> children);

  /**
   * 获取占位符值.
   *
   * @return 占位符对应的值 values
   */
  ChainList<Object> getValues();
}
