package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
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

  public static <T> Cnds<T> of(
      @Nonnull final Class<T> clazz,
      @Nonnull final QueryColumn<T> queryColumn,
      @Nonnull final LambdaCondition<T> condition) {
    final Cnds<T> cnds = new Cnds<>();
    cnds.clazz = clazz;
    cnds.condition = condition;
    return cnds;
  }

  @Override
  public <V> Cnds<T> eq(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.eq(true, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> ne(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.ne(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> gt(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.gt(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> ge(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.ge(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> lt(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.gt(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> le(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.le(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> like(
      final boolean cond, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    condition.like(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> notLike(
      final boolean cond, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    condition.notLike(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> between(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    condition.between(cond, field, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> notBetween(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    condition.notBetween(cond, field, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> isNull(final boolean cond, @Nonnull final IConsumer<T, V> field) {
    condition.isNull(cond, field);
    return this;
  }

  @Override
  public <V> Cnds<T> isNotNull(final boolean cond, @Nonnull final IConsumer<T, V> field) {
    condition.isNotNull(cond, field);
    return this;
  }

  @Override
  public <V> Cnds<T> in(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    condition.in(cond, field, values);
    return this;
  }

  @Override
  public <V> Cnds<T> notIn(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    condition.notIn(cond, field, values);
    return this;
  }

  @Override
  public <V> Cnds<T> orEq(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orEq(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orNe(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orNe(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orGt(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orGt(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orGe(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orGe(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLt(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orLt(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLe(final boolean cond, @Nonnull final IConsumer<T, V> field, final V value) {
    condition.orLe(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orLike(
      final boolean cond, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    condition.orLike(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotLike(
      final boolean cond, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    condition.orNotLike(cond, field, value);
    return this;
  }

  @Override
  public <V> Cnds<T> orBetween(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    condition.orBetween(cond, field, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotBetween(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    condition.orNotBetween(cond, field, start, end);
    return this;
  }

  @Override
  public <V> Cnds<T> orIsNull(final boolean cond, @Nonnull final IConsumer<T, V> field) {
    condition.orIsNull(cond, field);
    return this;
  }

  @Override
  public <V> Cnds<T> orIsNotNull(final boolean cond, @Nonnull final IConsumer<T, V> field) {
    condition.orIsNotNull(cond, field);
    return this;
  }

  @Override
  public <V> Cnds<T> orIn(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    condition.orIn(cond, field, values);
    return this;
  }

  @Override
  public <V> Cnds<T> orNotIn(
      final boolean cond,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    condition.orNotIn(cond, field, values);
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
  public final Cnds<T> groupBy(@Nonnull final IFunction<T, ?> field) {
    this.condition.groupBy(field);
    return this;
  }
}
