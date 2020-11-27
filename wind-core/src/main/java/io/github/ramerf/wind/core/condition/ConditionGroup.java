package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.util.Collection;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class ConditionGroup<T extends AbstractEntityPoJo<T, ?>> {
  @Getter private final LambdaCondition<T> condition;

  public static <T extends AbstractEntityPoJo<T, ?>> ConditionGroup<T> getInstance(
      final Class<T> clazz) {
    return new ConditionGroup<>(clazz);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> ConditionGroup<T> getInstance(
      final QueryColumn<T> queryColumn) {
    return new ConditionGroup<>(queryColumn);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> ConditionGroup<T> getInstance(
      final Class<T> clazz, String tableName, String tableAlia) {
    return new ConditionGroup<>(clazz, tableName, tableAlia);
  }

  private ConditionGroup(final Class<T> clazz) {
    this.condition = new LambdaCondition<>(clazz, null, null);
  }

  private ConditionGroup(final QueryColumn<T> queryColumn) {
    this.condition = new LambdaCondition<>(queryColumn);
  }

  private ConditionGroup(final Class<T> clazz, String tableName, String tableAlia) {
    this.condition = new LambdaCondition<>(clazz, tableName, tableAlia);
  }

  public <V> ConditionGroup<T> andEq(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.eq(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.eq(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andNe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ne(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ne(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andGt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.gt(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.gt(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andGe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ge(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ge(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.lt(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.lt(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.le(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.le(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.like(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.like(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andNotLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.notLike(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.notLike(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> andBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.between(true, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> andBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> andNotBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.notBetween(true, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> andNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> andIsNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.isNull(true, field);
    return this;
  }

  public <V> ConditionGroup<T> andIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNull(condition, field);
    return this;
  }

  public <V> ConditionGroup<T> andIsNotNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.isNotNull(true, field);
    return this;
  }

  public <V> ConditionGroup<T> andIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNotNull(condition, field);
    return this;
  }

  public <V> ConditionGroup<T> andIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.in(true, field, values);
    return this;
  }

  public <V> ConditionGroup<T> andIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, field, values);
    return this;
  }

  public <V> ConditionGroup<T> andNotIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.notIn(true, field, values);
    return this;
  }

  public <V> ConditionGroup<T> andNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, field, values);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> ConditionGroup<T> andEq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final QueryColumn<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    this.condition.eq(true, field, queryColumn, field2);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> ConditionGroup<T> andEq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    this.condition.eq(condition, field, queryColumn, field2);
    return this;
  }

  public ConditionGroup<T> andExists(@Nonnull final Condition<T> childConditions) {
    this.condition.exists(true, childConditions);
    return this;
  }

  public ConditionGroup<T> andExists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    this.condition.exists(condition, childConditions);
    return this;
  }

  public <V> ConditionGroup<T> orEq(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orEq(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orEq(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orNe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orNe(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orNe(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orGt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGt(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGt(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orGe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGe(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGe(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLt(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLt(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLe(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLe(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orLike(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orLike(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orNotLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orNotLike(true, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orNotLike(condition, field, value);
    return this;
  }

  public <V> ConditionGroup<T> orBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.orBetween(true, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> orBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> orNotBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.orNotBetween(true, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, field, start, end);
    return this;
  }

  public <V> ConditionGroup<T> orIsNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNull(true, field);
    return this;
  }

  public <V> ConditionGroup<T> orIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNull(condition, field);
    return this;
  }

  public <V> ConditionGroup<T> orIsNotNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNotNull(true, field);
    return this;
  }

  public <V> ConditionGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNotNull(condition, field);
    return this;
  }

  public <V> ConditionGroup<T> orIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.orIn(true, field, values);
    return this;
  }

  public <V> ConditionGroup<T> orIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, field, values);
    return this;
  }

  public <V> ConditionGroup<T> orNotIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.orNotIn(true, field, values);
    return this;
  }

  public <V> ConditionGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, field, values);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> ConditionGroup<T> orEq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final QueryColumn<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    this.condition.orEq(true, field, queryColumn, field2);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> ConditionGroup<T> orEq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    this.condition.orEq(condition, field, queryColumn, field2);
    return this;
  }

  public ConditionGroup<T> orExists(@Nonnull final Condition<T> childConditions) {
    this.condition.orExists(true, childConditions);
    return this;
  }

  public ConditionGroup<T> orExists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    this.condition.orExists(condition, childConditions);
    return this;
  }

  public ConditionGroup<T> and(@Nonnull ConditionGroup<T> group) {
    this.condition.and(group.getCondition());
    return this;
  }

  public ConditionGroup<T> or(@Nonnull ConditionGroup<T> group) {
    this.condition.or(group.getCondition());
    return this;
  }

  public boolean isEmpty() {
    return this.condition.valueTypes.size() <= 0;
  }

  /*
    ConditionGroup 的获取:
    1. 使用Consumer
    2. 传入Condition构造
    TODO-WARN 把LambdaCondition中的方法拷贝过来,实现直接用condition调用原方法
      LambdaContion添加方法or(ConditionGroup)/and(ConditionGroup),实现:获取ConditionGroup的condition,外加括号
      io.github.ramerf.wind.core.condition.LambdaCondition.or(boolean,
      io.github.ramerf.wind.core.condition.Condition<T>)方法签名改了就可以啦
  */
}
