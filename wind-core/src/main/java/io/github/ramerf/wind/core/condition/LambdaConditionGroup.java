package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.function.GetterFunction;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class LambdaConditionGroup<T>
    extends AbstractConditionGroup<T, LambdaConditionGroup<T>, LambdaCondition<T>>
    implements ILambdaCondition<T, LambdaConditionGroup<T>> {

  public static <T> LambdaConditionGroup<T> of(final LambdaCondition<T> lambdaCondition) {
    final LambdaConditionGroup<T> group = new LambdaConditionGroup<>();
    group.condition = new LambdaCondition<>();
    return group;
  }

  public static <T> LambdaConditionGroup<T> of(final Class<T> clazz) {
    return new LambdaConditionGroup<>(clazz);
  }

  private LambdaConditionGroup() {}

  private LambdaConditionGroup(final Class<T> clazz) {
    this.condition = new LambdaCondition<>(clazz);
  }

  @Override
  public <V> LambdaConditionGroup<T> eq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.eq(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> ne(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ne(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> gt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.gt(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> ge(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ge(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> lt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.lt(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> le(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.le(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> like(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.like(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.notLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> between(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> isNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNull(condition, setter);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> isNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> in(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, setter, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orEq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orEq(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orNe(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orGt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGt(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orGe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGe(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLt(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLe(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orNotLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIsNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNull(condition, setter);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, setter, values);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> and(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> or(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    this.condition.groupBy(getter);
    return this;
  }
}
