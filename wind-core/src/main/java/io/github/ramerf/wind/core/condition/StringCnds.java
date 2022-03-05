package io.github.ramerf.wind.core.condition;

import java.util.Collection;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * @author ramer
 * @since 24/07/2021
 */
public class StringCnds<T> extends AbstractCnd<T, StringCnds<T>, StringCondition<T>>
    implements IStringCondition<T, StringCnds<T>> {
  @Getter private StringCondition<T> condition;

  private StringCnds(final Class<T> clazz) {
    super(clazz);
  }

  public static <T> StringCnds<T> of(final Class<T> clazz) {
    final StringCnds<T> cnds = new StringCnds<>(clazz);
    cnds.condition = StringCondition.of(clazz);
    return cnds;
  }

  public static <T> StringCnds<T> of(
      @Nonnull final Class<T> clazz, @Nonnull final StringCondition<T> condition) {
    final StringCnds<T> cnds = new StringCnds<>(clazz);
    cnds.condition = condition;
    return cnds;
  }

  @Override
  public StringCnds<T> eq(final boolean cond, @Nonnull final String column, final Object value) {
    condition.eq(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> ne(final boolean cond, @Nonnull final String column, final Object value) {
    condition.ne(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> gt(final boolean cond, @Nonnull final String column, final Object value) {
    condition.gt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> ge(final boolean cond, @Nonnull final String column, final Object value) {
    condition.ge(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> lt(final boolean cond, @Nonnull final String column, final Object value) {
    condition.lt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> le(final boolean cond, @Nonnull final String column, final Object value) {
    condition.le(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> like(final boolean cond, @Nonnull final String column, final Object value) {
    condition.like(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> notLike(
      final boolean cond, @Nonnull final String column, final Object value) {
    condition.notLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> between(
      final boolean cond, @Nonnull final String column, final Object start, final Object end) {
    condition.between(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> notBetween(
      final boolean cond, @Nonnull final String column, final Object start, final Object end) {
    condition.notBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> isNull(final boolean cond, @Nonnull final String column) {
    condition.isNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> isNotNull(final boolean cond, @Nonnull final String column) {
    condition.isNotNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> in(
      final boolean cond, @Nonnull final String column, final Collection<?> values) {
    condition.in(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> notIn(
      final boolean cond, @Nonnull final String column, final Collection<?> values) {
    condition.notIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> orEq(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orEq(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orNe(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orNe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orGt(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orGt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orGe(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orGe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLt(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orLt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLe(final boolean cond, @Nonnull final String column, final Object value) {
    condition.orLe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLike(
      final boolean cond, @Nonnull final String column, final Object value) {
    condition.orLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orNotLike(
      final boolean cond, @Nonnull final String column, final Object value) {
    condition.orNotLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orBetween(
      final boolean cond, @Nonnull final String column, final Object start, final Object end) {
    condition.orBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> orNotBetween(
      final boolean cond, @Nonnull final String column, final Object start, final Object end) {
    condition.orNotBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> orIsNull(final boolean cond, @Nonnull final String column) {
    condition.orIsNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> orIsNotNull(final boolean cond, @Nonnull final String column) {
    condition.orIsNotNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> orIn(
      final boolean cond, @Nonnull final String column, final Collection<?> values) {
    condition.orIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> orNotIn(
      final boolean cond, @Nonnull final String column, final Collection<?> values) {
    condition.orNotIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> and(@Nonnull StringConditionGroup<T> group) {
    condition.and(group);
    return this;
  }

  @Override
  public StringCnds<T> or(@Nonnull StringConditionGroup<T> group) {
    condition.or(group);
    return this;
  }

  @Override
  public StringCnds<T> and(final String column, final String operator, final Object value) {
    condition.and(column, operator, value);
    return this;
  }

  @Override
  public StringCnds<T> or(final String column, final String operator, final Object value) {
    condition.or(column, operator, value);
    return this;
  }

  @Override
  public StringCnds<T> groupBy(@Nonnull final String column) {
    condition.groupBy(column);
    return this;
  }
}
