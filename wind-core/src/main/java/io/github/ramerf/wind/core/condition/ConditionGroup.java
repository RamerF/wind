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

  public <V> LambdaCondition<T> eq(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.eq(true, field, value);
  }

  public <V> LambdaCondition<T> eq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.eq(condition, field, value);
  }

  public <V> LambdaCondition<T> ne(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.ne(true, field, value);
  }

  public <V> LambdaCondition<T> ne(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.ne(condition, field, value);
  }

  public <V> LambdaCondition<T> gt(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.gt(true, field, value);
  }

  public <V> LambdaCondition<T> gt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.gt(condition, field, value);
  }

  public <V> LambdaCondition<T> ge(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.ge(true, field, value);
  }

  public <V> LambdaCondition<T> ge(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.ge(condition, field, value);
  }

  public <V> LambdaCondition<T> lt(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.lt(true, field, value);
  }

  public <V> LambdaCondition<T> lt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.lt(condition, field, value);
  }

  public <V> LambdaCondition<T> le(@Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.le(true, field, value);
  }

  public <V> LambdaCondition<T> le(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    return this.condition.le(condition, field, value);
  }

  public <V> LambdaCondition<T> like(@Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return this.condition.like(true, field, value);
  }

  public <V> LambdaCondition<T> like(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return this.condition.like(condition, field, value);
  }

  public <V> LambdaCondition<T> notLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return this.condition.notLike(true, field, value);
  }

  public <V> LambdaCondition<T> notLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    return this.condition.notLike(condition, field, value);
  }

  public <V> LambdaCondition<T> between(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return this.condition.between(true, field, start, end);
  }

  public <V> LambdaCondition<T> between(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    return this.condition.between(condition, field, start, end);
  }

  public <V> LambdaCondition<T> notBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    return this.condition.notBetween(true, field, start, end);
  }

  public <V> LambdaCondition<T> notBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    return this.condition.notBetween(condition, field, start, end);
  }

  public <V> LambdaCondition<T> isNull(@Nonnull final IConsumer<T, V> field) {
    return this.condition.isNull(true, field);
  }

  public <V> LambdaCondition<T> isNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    return this.condition.isNull(condition, field);
  }

  public <V> LambdaCondition<T> isNotNull(@Nonnull final IConsumer<T, V> field) {
    return this.condition.isNotNull(true, field);
  }

  public <V> LambdaCondition<T> isNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    return this.condition.isNotNull(condition, field);
  }

  public <V> LambdaCondition<T> in(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return this.condition.in(true, field, values);
  }

  public <V> LambdaCondition<T> in(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    return this.condition.in(condition, field, values);
  }

  public <V> LambdaCondition<T> notIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    return this.condition.notIn(true, field, values);
  }

  public <V> LambdaCondition<T> notIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    return this.condition.notIn(condition, field, values);
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> eq(
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final QueryColumn<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return this.condition.eq(true, field, queryColumn, field2);
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>> LambdaCondition<T> eq(
      final boolean condition,
      @Nonnull final IFunction<T, ?> field,
      @Nonnull final AbstractQueryEntity<Q> queryColumn,
      @Nonnull final IFunction<R, ?> field2) {
    return this.condition.eq(condition, field, queryColumn, field2);
  }

  public LambdaCondition<T> exists(@Nonnull final Condition<T> childConditions) {
    return this.condition.exists(true, childConditions);
  }

  public LambdaCondition<T> exists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    return this.condition.exists(condition, childConditions);
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
