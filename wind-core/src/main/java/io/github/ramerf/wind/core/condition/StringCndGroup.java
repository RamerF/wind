package io.github.ramerf.wind.core.condition;

import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * 基于 {@link StringCnd} 的分组.
 *
 * @author ramer
 * @since 15/08/2021
 */
public class StringCndGroup<T> extends AbstractCndGroup<T, StringCndGroup<T>, StringCnd<T>>
    implements IStringCondition<T, StringCndGroup<T>> {
  public static <T> StringCndGroup<T> of(final StringCnd<T> stringCondition) {
    final StringCndGroup<T> group = new StringCndGroup<>();
    group.condition = new StringCnd<>();
    return group;
  }

  public static <T> StringCndGroup<T> of(final Class<T> clazz) {
    return new StringCndGroup<>(clazz);
  }

  private StringCndGroup() {}

  private StringCndGroup(final Class<T> clazz) {
    this.condition = new StringCnd<>(clazz);
  }

  @Override
  public StringCndGroup<T> eq(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.eq(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> ne(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.ne(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> gt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.gt(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> ge(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.ge(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> lt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.lt(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> le(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.le(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> like(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.like(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> notLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.notLike(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> between(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.between(condition, column, start, end);
    return this;
  }

  @Override
  public StringCndGroup<T> notBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.notBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringCndGroup<T> isNull(final boolean condition, @Nonnull final String column) {
    this.condition.isNull(condition, column);
    return this;
  }

  @Override
  public StringCndGroup<T> isNotNull(final boolean condition, @Nonnull final String column) {
    this.condition.isNotNull(condition, column);
    return this;
  }

  @Override
  public StringCndGroup<T> in(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.in(condition, column, values);
    return this;
  }

  @Override
  public StringCndGroup<T> notIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.notIn(condition, column, values);
    return this;
  }

  @Override
  public StringCndGroup<T> orEq(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orEq(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orNe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orNe(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orGt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orGt(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orGe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orGe(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orLt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLt(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orLe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLe(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLike(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orNotLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orNotLike(condition, column, value);
    return this;
  }

  @Override
  public StringCndGroup<T> orBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.orBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringCndGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.orNotBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringCndGroup<T> orIsNull(final boolean condition, @Nonnull final String column) {
    this.condition.orIsNull(condition, column);
    return this;
  }

  @Override
  public StringCndGroup<T> orIsNotNull(final boolean condition, @Nonnull final String column) {
    this.condition.orIsNotNull(condition, column);
    return this;
  }

  @Override
  public StringCndGroup<T> orIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.orIn(condition, column, values);
    return this;
  }

  @Override
  public StringCndGroup<T> orNotIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.orNotIn(condition, column, values);
    return this;
  }

  @Override
  public StringCndGroup<T> and(@Nonnull final StringCndGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public StringCndGroup<T> or(@Nonnull final StringCndGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public StringCndGroup<T> and(final String left, final String operator, final Object right) {
    this.condition.and(left, operator, right);
    return this;
  }

  @Override
  public StringCndGroup<T> or(final String left, final String operator, final Object right) {
    this.condition.or(left, operator, right);
    return this;
  }

  @Override
  public StringCndGroup<T> groupBy(@Nonnull final String column) {
    this.condition.groupBy(column);
    return this;
  }
}
