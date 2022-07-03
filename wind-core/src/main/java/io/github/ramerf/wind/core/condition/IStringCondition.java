package io.github.ramerf.wind.core.condition;

import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * 条件构造.
 *
 * @author ramer
 * @since 2019/12/26
 */
public interface IStringCondition<T, U extends IStringCondition<T, U>> extends Condition<T, U> {

  default U eq(@Nonnull final String column, @Nonnull final Object value) {
    return eq(true, column, value);
  }

  U eq(final boolean condition, @Nonnull final String column, final Object value);

  default U ne(@Nonnull final String column, @Nonnull final Object value) {
    return ne(true, column, value);
  }

  U ne(final boolean condition, @Nonnull final String column, final Object value);

  default U gt(@Nonnull final String column, @Nonnull final Object value) {
    return gt(true, column, value);
  }

  U gt(final boolean condition, @Nonnull final String column, final Object value);

  default U ge(@Nonnull final String column, @Nonnull final Object value) {
    return ge(true, column, value);
  }

  U ge(final boolean condition, @Nonnull final String column, final Object value);

  default U lt(@Nonnull final String column, @Nonnull final Object value) {
    return lt(true, column, value);
  }

  U lt(final boolean condition, @Nonnull final String column, final Object value);

  default U le(@Nonnull final String column, @Nonnull final Object value) {
    return le(true, column, value);
  }

  U le(final boolean condition, @Nonnull final String column, final Object value);

  default U like(@Nonnull final String column, @Nonnull final Object value) {
    return like(true, column, value);
  }

  U like(final boolean condition, @Nonnull final String column, final Object value);

  default U notLike(@Nonnull final String column, @Nonnull final Object value) {
    return notLike(true, column, value);
  }

  U notLike(final boolean condition, @Nonnull final String column, final Object value);

  default U between(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return between(true, column, start, end);
  }

  U between(
      final boolean condition, @Nonnull final String column, final Object start, final Object end);

  default U notBetween(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return notBetween(true, column, start, end);
  }

  U notBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end);

  default U isNull(@Nonnull final String column) {
    return isNull(true, column);
  }

  U isNull(final boolean condition, @Nonnull final String column);

  default U isNotNull(@Nonnull final String column) {
    return isNotNull(true, column);
  }

  U isNotNull(final boolean condition, final String column);

  default U in(@Nonnull final String column, @Nonnull final Collection<?> values) {
    return in(true, column, values);
  }

  U in(final boolean condition, @Nonnull final String column, final Collection<?> values);

  default U notIn(@Nonnull final String column, @Nonnull final Collection<?> values) {
    return notIn(true, column, values);
  }

  U notIn(final boolean condition, @Nonnull final String column, final Collection<?> values);

  default U orEq(@Nonnull final String column, @Nonnull final Object value) {
    return orEq(true, column, value);
  }

  U orEq(final boolean condition, @Nonnull final String column, final Object value);

  default U orNe(@Nonnull final String column, @Nonnull final Object value) {
    return orNe(true, column, value);
  }

  U orNe(final boolean condition, @Nonnull final String column, final Object value);

  default U orGt(@Nonnull final String column, @Nonnull final Object value) {
    return orGt(true, column, value);
  }

  U orGt(final boolean condition, @Nonnull final String column, final Object value);

  default U orGe(@Nonnull final String column, @Nonnull final Object value) {
    return orGe(true, column, value);
  }

  U orGe(final boolean condition, @Nonnull final String column, final Object value);

  default U orLt(@Nonnull final String column, @Nonnull final Object value) {
    return orLt(true, column, value);
  }

  U orLt(final boolean condition, @Nonnull final String column, final Object value);

  default U orLe(@Nonnull final String column, @Nonnull final Object value) {
    return orLe(true, column, value);
  }

  U orLe(final boolean condition, @Nonnull final String column, final Object value);

  default U orLike(@Nonnull final String column, @Nonnull final Object value) {
    return orLike(true, column, value);
  }

  U orLike(final boolean condition, @Nonnull final String column, final Object value);

  default U orNotLike(@Nonnull final String column, @Nonnull final Object value) {
    return orNotLike(true, column, value);
  }

  U orNotLike(final boolean condition, @Nonnull final String column, final Object value);

  default U orBetween(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return orBetween(true, column, start, end);
  }

  U orBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end);

  default U orNotBetween(
      @Nonnull final String column, @Nonnull final Object start, @Nonnull final Object end) {
    return orNotBetween(true, column, start, end);
  }

  U orNotBetween(
      final boolean condition, @Nonnull final String column, final Object start, final Object end);

  default U orIsNull(@Nonnull final String column) {
    return orIsNull(true, column);
  }

  U orIsNull(final boolean condition, @Nonnull final String column);

  default U orIsNotNull(@Nonnull final String column) {
    return orIsNotNull(true, column);
  }

  U orIsNotNull(final boolean condition, final String column);

  default U orIn(@Nonnull final String column, @Nonnull final Collection<?> values) {
    return orIn(true, column, values);
  }

  U orIn(final boolean condition, @Nonnull final String column, final Collection<?> values);

  default U orNotIn(@Nonnull final String column, @Nonnull final Collection<?> values) {
    return orNotIn(true, column, values);
  }

  U orNotIn(final boolean condition, @Nonnull final String column, final Collection<?> values);

  U and(@Nonnull StringCndGroup<T> group);

  U or(@Nonnull StringCndGroup<T> group);

  default U and(final String left, final MatchPattern operator, final Object right) {
    return and(left, operator.operator, right);
  }

  U and(final String left, final String operator, final Object right);

  default U or(final String left, final MatchPattern operator, final Object right) {
    return or(left, operator.operator, right);
  }

  U or(final String left, final String operator, final Object right);

  U groupBy(@Nonnull final String column);
}
