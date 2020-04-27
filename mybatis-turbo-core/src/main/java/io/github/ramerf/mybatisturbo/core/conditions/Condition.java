package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import java.io.Serializable;
import java.util.Collection;

/**
 * @author Tang Xiaofeng
 * @since 20-1-6
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public interface Condition<T extends AbstractEntity> extends Predicate<T> {
  Conditions<T> eq(final IFunction<T, ?> field, final Object value);

  Conditions<T> eq(final boolean condition, final IFunction<T, ?> field, final Object value);

  /** 连表条件. */
  <R extends AbstractEntity, Q extends AbstractEntity> Conditions<T> eq(
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  /** 连表条件. */
  <R extends AbstractEntity, Q extends AbstractEntity> Conditions<T> eq(
      final boolean condition,
      final IFunction<T, ?> field,
      final AbstractQueryEntity<Q> queryColumn,
      final IFunction<R, ?> field2);

  Conditions<T> ne(final IFunction<T, ?> field, final Object value);

  Conditions<T> ne(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> gt(final IFunction<T, ?> field, final Object value);

  Conditions<T> gt(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> ge(final IFunction<T, ?> field, final Object value);

  Conditions<T> ge(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> lt(final IFunction<T, ?> field, final Object value);

  Conditions<T> lt(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> le(final IFunction<T, ?> field, final Object value);

  Conditions<T> le(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> like(final IFunction<T, ?> field, final Object value);

  Conditions<T> like(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> likeLeft(final IFunction<T, ?> field, final Object value);

  Conditions<T> likeLeft(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> likeRight(final IFunction<T, ?> field, final Object value);

  Conditions<T> likeRight(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> notLike(final IFunction<T, ?> field, final Object value);

  Conditions<T> notLike(final boolean condition, final IFunction<T, ?> field, final Object value);

  Conditions<T> between(final IFunction<T, ?> field, final Object start, final Object end);

  Conditions<T> between(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  Conditions<T> notBetween(final IFunction<T, ?> field, final Object start, final Object end);

  Conditions<T> notBetween(
      final boolean condition, final IFunction<T, ?> field, final Object start, final Object end);

  Conditions<T> isNull(final IFunction<T, ?> field);

  Conditions<T> isNull(final boolean condition, final IFunction<T, ?> field);

  Conditions<T> isNotNull(final IFunction<T, ?> field);

  Conditions<T> isNotNull(final boolean condition, final IFunction<T, ?> field);

  Conditions<T> exists(final Conditions<T> childConditions);

  // TODO-WARN 很明显这里的拼接有问题,要传递的参数是(Query+Condition)最上层的接口,能够获取到每个段的sql.
  //  因为涉及到整个模式调整,暂时不动
  Conditions<T> exists(final boolean condition, final Conditions<T> childConditions);

  Conditions<T> in(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  Conditions<T> in(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  Conditions<T> notIn(final IFunction<T, ?> field, final Collection<? extends Serializable> values);

  Conditions<T> notIn(
      final boolean condition,
      final IFunction<T, ?> field,
      final Collection<? extends Serializable> values);

  Conditions<T> and(Conditions<T> children);

  Conditions<T> or(Conditions<T> children);
}
