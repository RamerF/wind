package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.GetterFunction;
import io.github.ramerf.wind.core.function.SetterFunction;
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
  default <V> U eq(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return eq(true, setter, value);
  }

  <V> U eq(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U ne(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return ne(true, setter, value);
  }

  <V> U ne(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U gt(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return gt(true, setter, value);
  }

  <V> U gt(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U ge(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return ge(true, setter, value);
  }

  <V> U ge(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U lt(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return lt(true, setter, value);
  }

  <V> U lt(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U le(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return le(true, setter, value);
  }

  <V> U le(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U like(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return like(true, setter, value);
  }

  <V> U like(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U notLike(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return notLike(true, setter, value);
  }

  <V> U notLike(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U between(@Nonnull SetterFunction<T, V> setter, @Nonnull V start, @Nonnull V end) {
    return between(true, setter, start, end);
  }

  <V> U between(boolean condition, @Nonnull SetterFunction<T, V> setter, V start, V end);

  default <V> U notBetween(@Nonnull SetterFunction<T, V> setter, @Nonnull V start, @Nonnull V end) {
    return notBetween(true, setter, start, end);
  }

  <V> U notBetween(boolean condition, @Nonnull SetterFunction<T, V> setter, V start, V end);

  default <V> U isNull(@Nonnull SetterFunction<T, V> setter) {
    return isNull(true, setter);
  }

  <V> U isNull(boolean condition, @Nonnull SetterFunction<T, V> setter);

  default <V> U isNotNull(@Nonnull SetterFunction<T, V> setter) {
    return isNotNull(true, setter);
  }

  <V> U isNotNull(boolean condition, @Nonnull SetterFunction<T, V> setter);

  default <V> U in(@Nonnull SetterFunction<T, V> setter, @Nonnull Collection<V> values) {
    return in(true, setter, values);
  }

  <V> U in(boolean condition, @Nonnull SetterFunction<T, V> setter, Collection<V> values);

  default <V> U notIn(@Nonnull SetterFunction<T, V> setter, @Nonnull Collection<V> values) {
    return notIn(true, setter, values);
  }

  <V> U notIn(boolean condition, @Nonnull SetterFunction<T, V> setter, Collection<V> values);

  default <V> U orEq(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orEq(true, setter, value);
  }

  <V> U orEq(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orNe(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orNe(true, setter, value);
  }

  <V> U orNe(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orGt(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orGt(true, setter, value);
  }

  <V> U orGt(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orGe(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orGe(true, setter, value);
  }

  <V> U orGe(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orLt(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orLt(true, setter, value);
  }

  <V> U orLt(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orLe(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orLe(true, setter, value);
  }

  <V> U orLe(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orLike(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orLike(true, setter, value);
  }

  <V> U orLike(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orNotLike(@Nonnull SetterFunction<T, V> setter, @Nonnull V value) {
    return orNotLike(true, setter, value);
  }

  <V> U orNotLike(boolean condition, @Nonnull SetterFunction<T, V> setter, V value);

  default <V> U orBetween(@Nonnull SetterFunction<T, V> setter, @Nonnull V start, @Nonnull V end) {
    return orBetween(true, setter, start, end);
  }

  <V> U orBetween(boolean condition, @Nonnull SetterFunction<T, V> setter, V start, V end);

  default <V> U orNotBetween(
      @Nonnull SetterFunction<T, V> setter, @Nonnull V start, @Nonnull V end) {
    return orNotBetween(true, setter, start, end);
  }

  <V> U orNotBetween(boolean condition, @Nonnull SetterFunction<T, V> setter, V start, V end);

  default <V> U orIsNull(@Nonnull SetterFunction<T, V> setter) {
    return orIsNull(true, setter);
  }

  <V> U orIsNull(boolean condition, @Nonnull SetterFunction<T, V> setter);

  default <V> U orIsNotNull(@Nonnull SetterFunction<T, V> setter) {
    return orIsNotNull(true, setter);
  }

  <V> U orIsNotNull(boolean condition, @Nonnull SetterFunction<T, V> setter);

  default <V> U orIn(@Nonnull SetterFunction<T, V> setter, @Nonnull Collection<V> values) {
    return orIn(true, setter, values);
  }

  <V> U orIn(boolean condition, @Nonnull SetterFunction<T, V> setter, Collection<V> values);

  default <V> U orNotIn(@Nonnull SetterFunction<T, V> setter, @Nonnull Collection<V> values) {
    return orNotIn(true, setter, values);
  }

  <V> U orNotIn(boolean condition, @Nonnull SetterFunction<T, V> setter, Collection<V> values);

  U and(@Nonnull CndGroup<T> group);

  U or(@Nonnull CndGroup<T> group);

  U groupBy(@Nonnull final GetterFunction<T, ?> getter);
}
