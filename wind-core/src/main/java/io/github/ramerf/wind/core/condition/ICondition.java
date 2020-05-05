package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.io.Serializable;
import java.util.Collection;

/**
 * @author Tang Xiaofeng
 * @since 20-1-6
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public interface ICondition<T extends AbstractEntity> extends Predicate<T> {
  <R> Condition<T> eq(final IConsumer<T, R> field, final R value);

  <R> Condition<T> eq(final boolean condition, final IConsumer<T, R> field, final R value);

  Condition<T> eq(final IFunction<T, ?> field, final Object value);

  Condition<T> eq(final boolean condition, final IFunction<T, ?> field, final Object value);

  /** 连表条件. */
  <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  /** 连表条件. */
  <R extends AbstractEntity, Q extends AbstractEntity> Condition<T> eq(
      final boolean condition,
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  Condition<T> ne(final IFunction<T, ?> field, final Object value);

  Condition<T> ne(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> gt(final IFunction<T, ?> field, final Object value);

  Condition<T> gt(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> ge(final IFunction<T, ?> field, final Object value);

  Condition<T> ge(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> lt(final IFunction<T, ?> field, final Object value);

  Condition<T> lt(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> le(final IFunction<T, ?> field, final Object value);

  Condition<T> le(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> like(final IFunction<T, ?> field, final Object value);

  Condition<T> like(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> likeLeft(final IFunction<T, ?> field, final Object value);

  Condition<T> likeLeft(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> likeRight(final IFunction<T, ?> field, final Object value);

  Condition<T> likeRight(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> notLike(final IFunction<T, ?> field, final Object value);

  Condition<T> notLike(final boolean condition, final IFunction<T, ?> field, final Object value);

  Condition<T> between(final IFunction<T, ?> field, final Object start, final Object end);

  Condition<T> between(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  Condition<T> notBetween(final IFunction<T, ?> field, final Object start, final Object end);

  Condition<T> notBetween(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  Condition<T> isNull(final IFunction<T, ?> field);

  Condition<T> isNull(final boolean condition, final IFunction<T, ?> field);

  Condition<T> isNotNull(final IFunction<T, ?> field);

  Condition<T> isNotNull(final boolean condition, final IFunction<T, ?> field);

  Condition<T> exists(final Condition<T> childConditions);

  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动
  Condition<T> exists(final boolean condition, final Condition<T> childConditions);

  Condition<T> in(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  Condition<T> in(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  Condition<T> notIn(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  Condition<T> notIn(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  Condition<T> and(Condition<T> children);

  Condition<T> or(Condition<T> children);
}
