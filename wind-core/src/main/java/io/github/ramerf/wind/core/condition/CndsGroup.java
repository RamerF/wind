package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.function.SetterFunction;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class CndsGroup<T> extends AbstractConditionGroup<T, CndsGroup<T>, Cnd<T>>
    implements ILambdaCondition<T, CndsGroup<T>> {

  public static <T> CndsGroup<T> of(final Cnd<T> cnds) {
    final CndsGroup<T> group = new CndsGroup<>();
    group.condition = new Cnd<>();
    return group;
  }

  public static <T> CndsGroup<T> of(final Class<T> clazz) {
    return new CndsGroup<>(clazz);
  }

  private CndsGroup() {}

  private CndsGroup(final Class<T> clazz) {
    this.condition = new Cnd<>(clazz);
  }

  @Override
  public <V> CndsGroup<T> eq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.eq(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> ne(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ne(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> gt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.gt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> ge(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.ge(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> lt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.lt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> le(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.le(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> like(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.like(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> notLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.notLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> between(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndsGroup<T> notBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndsGroup<T> isNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndsGroup<T> isNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.isNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndsGroup<T> in(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndsGroup<T> notIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orEq(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orEq(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orNe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orNe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orGt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orGe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orGe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orLt(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLt(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orLe(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, final V value) {
    this.condition.orLe(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orNotLike(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter, @Nonnull final V value) {
    this.condition.orNotLike(condition, setter, value);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, setter, start, end);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orIsNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final SetterFunction<T, V> setter) {
    this.condition.orIsNotNull(condition, setter);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, setter, values);
    return this;
  }

  @Override
  public <V> CndsGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final SetterFunction<T, V> setter,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, setter, values);
    return this;
  }

  @Override
  public CndsGroup<T> and(@Nonnull CndsGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public CndsGroup<T> or(@Nonnull CndsGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public CndsGroup<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    this.condition.groupBy(getter);
    return this;
  }
}
