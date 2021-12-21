package io.github.ramerf.wind.core.condition;

import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * @author ramer
 * @since 15/08/2021
 */
public class StringConditionGroup<T>
    extends AbstractConditionGroup<T, StringConditionGroup<T>, StringCondition<T>>
    implements IStringCondition<T, StringConditionGroup<T>> {

  public static <T> StringConditionGroup<T> of(final StringCondition<T> stringCondition) {
    final StringConditionGroup<T> group = new StringConditionGroup<>();
    group.condition = new StringCondition<>();
    return group;
  }

  public static <T> StringConditionGroup<T> of(final Class<T> clazz) {
    return new StringConditionGroup<>(clazz);
  }

  private StringConditionGroup() {}

  private StringConditionGroup(final Class<T> clazz) {
    this.condition = new StringCondition<>(clazz);
  }

  @Override
  public StringConditionGroup<T> eq(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.eq(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> ne(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.ne(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> gt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.gt(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> ge(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.ge(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> lt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.lt(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> le(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.le(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> like(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.like(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> notLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.notLike(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> between(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.between(condition, column, start, end);
    return this;
  }

  @Override
  public StringConditionGroup<T> notBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.notBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringConditionGroup<T> isNull(final boolean condition, @Nonnull final String column) {
    this.condition.isNull(condition, column);
    return this;
  }

  @Override
  public StringConditionGroup<T> isNotNull(final boolean condition, @Nonnull final String column) {
    this.condition.isNotNull(condition, column);
    return this;
  }

  @Override
  public StringConditionGroup<T> in(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.in(condition, column, values);
    return this;
  }

  @Override
  public StringConditionGroup<T> notIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.notIn(condition, column, values);
    return this;
  }

  @Override
  public StringConditionGroup<T> orEq(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orEq(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orNe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orNe(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orGt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orGt(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orGe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orGe(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orLt(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLt(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orLe(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLe(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orLike(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orNotLike(
      final boolean condition, @Nonnull final String column, @Nonnull final Object value) {
    this.condition.orNotLike(condition, column, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> orBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.orBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringConditionGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    this.condition.orNotBetween(condition, column, start, end);
    return this;
  }

  @Override
  public StringConditionGroup<T> orIsNull(final boolean condition, @Nonnull final String column) {
    this.condition.orIsNull(condition, column);
    return this;
  }

  @Override
  public StringConditionGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final String column) {
    this.condition.orIsNotNull(condition, column);
    return this;
  }

  @Override
  public StringConditionGroup<T> orIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.orIn(condition, column, values);
    return this;
  }

  @Override
  public StringConditionGroup<T> orNotIn(
      final boolean condition, @Nonnull final String column, @Nonnull final Collection<?> values) {
    this.condition.orNotIn(condition, column, values);
    return this;
  }

  @Override
  public StringConditionGroup<T> and(@Nonnull final StringConditionGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public StringConditionGroup<T> or(@Nonnull final StringConditionGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public StringConditionGroup<T> and(
      final String column, final String operator, final Object value) {
    this.condition.and(column, operator, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> or(
      final String column, final String operator, final Object value) {
    this.condition.or(column, operator, value);
    return this;
  }

  @Override
  public StringConditionGroup<T> groupBy(@Nonnull final String column) {
    this.condition.groupBy(column);
    return this;
  }
}
