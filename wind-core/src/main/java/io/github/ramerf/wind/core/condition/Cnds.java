package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.SetterFunction;
import io.github.ramerf.wind.core.function.GetterFunction;
import java.util.Collection;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * @author ramer
 * @since 24/07/2021
 */
public class Cnds<T> extends AbstractCnd<T, Cnds<T>, LambdaCondition<T>>
    implements ILambdaCondition<T, Cnds<T>> {
  @Getter private Class<T> clazz;
  @Getter private LambdaCondition<T> condition;

  private Cnds() {}

  /**
   * Of cnds.
   *
   * @param <T> the type parameter
   * @param clazz 查询对象
   * @return the cnds
   */
  @Nonnull
  public static <T> Cnds<T> of(final Class<T> clazz) {
    final Cnds<T> cnds = new Cnds<>();
    cnds.clazz = clazz;
    cnds.condition = LambdaCondition.of(clazz);
    return cnds;
  }

  public static <T> Cnds<T> of(
      @Nonnull final Class<T> clazz, @Nonnull final LambdaCondition<T> condition) {
    final Cnds<T> cnds = new Cnds<>();
    cnds.clazz = clazz;
    cnds.condition = condition;
    return cnds;
  }

  @Override
  public <V> Cnds<T> eq(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.eq(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> ne(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.ne(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> gt(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.gt(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> ge(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.ge(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> lt(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.lt(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> le(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.le(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> like(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.like(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> notLike(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.notLike(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> between(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V start, final V end) {
    condition.between(cond, setter, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> notBetween(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V start, final V end) {
    condition.notBetween(cond, setter, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> isNull(final boolean cond, @Nonnull final SetterFunction<T, V> setter) {
    condition.isNull(cond, setter);
    return this;
  }

  @Override
  public <V> Cnds<T> isNotNull(final boolean cond, @Nonnull final SetterFunction<T, V> setter) {
    condition.isNotNull(cond, setter);
    return this;
  }

  @Override
  public <V> Cnds<T> in(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final Collection<V> values) {
    condition.in(cond, setter, values);
    return this;
  }

  @Override
  public <V> Cnds<T> notIn(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final Collection<V> values) {
    condition.notIn(cond, setter, values);
    return this;
  }

  @Override
  public <V> Cnds<T> orEq(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orEq(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orNe(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orNe(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orGt(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orGt(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orGe(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orGe(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLt(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orLt(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLe(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orLe(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLike(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orLike(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotLike(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V value) {
    condition.orNotLike(cond, setter, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orBetween(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V start, final V end) {
    condition.orBetween(cond, setter, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotBetween(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final V start, final V end) {
    condition.orNotBetween(cond, setter, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> orIsNull(final boolean cond, @Nonnull final SetterFunction<T, V> setter) {
    condition.orIsNull(cond, setter);
    return this;
  }

  @Override
  public <V> Cnds<T> orIsNotNull(final boolean cond, @Nonnull final SetterFunction<T, V> setter) {
    condition.orIsNotNull(cond, setter);
    return this;
  }

  @Override
  public <V> Cnds<T> orIn(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final Collection<V> values) {
    condition.orIn(cond, setter, values);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotIn(
      final boolean cond, @Nonnull final SetterFunction<T, V> setter, final Collection<V> values) {
    condition.orNotIn(cond, setter, values);
    return this;
  }

  @Override
  public Cnds<T> and(@Nonnull LambdaConditionGroup<T> group) {
    condition.and(group);
    return this;
  }

  @Override
  public Cnds<T> or(@Nonnull LambdaConditionGroup<T> group) {
    condition.or(group);
    return this;
  }

  @Override
  public final Cnds<T> groupBy(@Nonnull final GetterFunction<T, ?> getter) {
    this.condition.groupBy(getter);
    return this;
  }
}
