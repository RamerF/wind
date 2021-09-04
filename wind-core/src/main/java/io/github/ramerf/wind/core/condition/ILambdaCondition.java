package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * The interface Lambda condition.
 *
 * @param <T> pojo
 * @param <U> 当前对象
 * @since 15 /08/2021
 * @author ramer
 */
public interface ILambdaCondition<T, U extends ILambdaCondition<T, U>> extends Condition<T, U> {
  default <V> U eq(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return eq(true, field, value);
  }

  <V> U eq(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U ne(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return ne(true, field, value);
  }

  <V> U ne(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U gt(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return gt(true, field, value);
  }

  <V> U gt(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U ge(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return ge(true, field, value);
  }

  <V> U ge(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U lt(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return lt(true, field, value);
  }

  <V> U lt(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U le(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return le(true, field, value);
  }

  <V> U le(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U like(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return like(true, field, value);
  }

  <V> U like(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U notLike(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return notLike(true, field, value);
  }

  <V> U notLike(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U between(@Nonnull IConsumer<T, V> field, @Nonnull V start, @Nonnull V end) {
    return between(true, field, start, end);
  }

  <V> U between(boolean condition, @Nonnull IConsumer<T, V> field, V start, V end);

  default <V> U notBetween(@Nonnull IConsumer<T, V> field, @Nonnull V start, @Nonnull V end) {
    return notBetween(true, field, start, end);
  }

  <V> U notBetween(boolean condition, @Nonnull IConsumer<T, V> field, V start, V end);

  default <V> U isNull(@Nonnull IConsumer<T, V> field) {
    return isNull(true, field);
  }

  <V> U isNull(boolean condition, @Nonnull IConsumer<T, V> field);

  default <V> U isNotNull(@Nonnull IConsumer<T, V> field) {
    return isNotNull(true, field);
  }

  <V> U isNotNull(boolean condition, @Nonnull IConsumer<T, V> field);

  default <V> U in(@Nonnull IConsumer<T, V> field, @Nonnull Collection<V> values) {
    return in(true, field, values);
  }

  <V> U in(boolean condition, @Nonnull IConsumer<T, V> field, Collection<V> values);

  default <V> U notIn(@Nonnull IConsumer<T, V> field, @Nonnull Collection<V> values) {
    return notIn(true, field, values);
  }

  <V> U notIn(boolean condition, @Nonnull IConsumer<T, V> field, Collection<V> values);

  default <V> U orEq(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orEq(true, field, value);
  }

  <V> U orEq(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orNe(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orNe(true, field, value);
  }

  <V> U orNe(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orGt(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orGt(true, field, value);
  }

  <V> U orGt(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orGe(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orGe(true, field, value);
  }

  <V> U orGe(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orLt(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orLt(true, field, value);
  }

  <V> U orLt(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orLe(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orLe(true, field, value);
  }

  <V> U orLe(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orLike(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orLike(true, field, value);
  }

  <V> U orLike(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orNotLike(@Nonnull IConsumer<T, V> field, @Nonnull V value) {
    return orNotLike(true, field, value);
  }

  <V> U orNotLike(boolean condition, @Nonnull IConsumer<T, V> field, V value);

  default <V> U orBetween(@Nonnull IConsumer<T, V> field, @Nonnull V start, @Nonnull V end) {
    return orBetween(true, field, start, end);
  }

  <V> U orBetween(boolean condition, @Nonnull IConsumer<T, V> field, V start, V end);

  default <V> U orNotBetween(@Nonnull IConsumer<T, V> field, @Nonnull V start, @Nonnull V end) {
    return orNotBetween(true, field, start, end);
  }

  <V> U orNotBetween(boolean condition, @Nonnull IConsumer<T, V> field, V start, V end);

  default <V> U orIsNull(@Nonnull IConsumer<T, V> field) {
    return orIsNull(true, field);
  }

  <V> U orIsNull(boolean condition, @Nonnull IConsumer<T, V> field);

  default <V> U orIsNotNull(@Nonnull IConsumer<T, V> field) {
    return orIsNotNull(true, field);
  }

  <V> U orIsNotNull(boolean condition, @Nonnull IConsumer<T, V> field);

  default <V> U orIn(@Nonnull IConsumer<T, V> field, @Nonnull Collection<V> values) {
    return orIn(true, field, values);
  }

  <V> U orIn(boolean condition, @Nonnull IConsumer<T, V> field, Collection<V> values);

  default <V> U orNotIn(@Nonnull IConsumer<T, V> field, @Nonnull Collection<V> values) {
    return orNotIn(true, field, values);
  }

  <V> U orNotIn(boolean condition, @Nonnull IConsumer<T, V> field, Collection<V> values);

  U and(@Nonnull LambdaConditionGroup<T> group);

  U or(@Nonnull LambdaConditionGroup<T> group);

  U groupBy(@Nonnull final IFunction<T, ?> field);
}
