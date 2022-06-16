package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.function.SetterFunction;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * 基于 {@link Cnd} 的分组.
 *
 * @author ramer
 * @since 24/11/2020
 */
public class CndGroup<T> extends AbstractCndGroup<T, CndGroup<T>, Cnd<T>>
    implements ILambdaCondition<T, CndGroup<T>> {
  public static <T> CndGroup<T> of(final Cnd<T> cnds) {
    final CndGroup<T> group = new CndGroup<>();
    group.condition = new Cnd<>();
    return group;
  }

  public static <T> CndGroup<T> of(final Class<T> clazz) {
    return new CndGroup<>(clazz);
  }

  private CndGroup() {}

  private CndGroup(final Class<T> clazz) {
    this.condition = new Cnd<>(clazz);
  }

  @Override
  public <V> CndGroup<T> eq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.eq(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> ne(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ne(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> gt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.gt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> ge(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ge(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> lt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.lt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> le(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.le(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> like(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final String value) {
    this.condition.like(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> notLike(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final String value) {
    this.condition.notLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> between(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndGroup<T> notBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndGroup<T> isNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndGroup<T> isNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndGroup<T> in(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndGroup<T> notIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndGroup<T> orEq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orEq(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orNe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orNe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orGt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orGe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orLt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orLe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orNotLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orNotLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndGroup<T> orBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndGroup<T> orIsNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndGroup<T> orIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, setter, values);
    return this;
  }

  @Override
  public CndGroup<T> and(@Nonnull CndGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public CndGroup<T> or(@Nonnull CndGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public CndGroup<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    this.condition.groupBy(getter);
    return this;
  }
}
