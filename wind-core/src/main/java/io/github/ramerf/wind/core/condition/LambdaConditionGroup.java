package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.util.Collection;
import javax.annotation.Nonnull;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class LambdaConditionGroup<T>
    extends AbstractConditionGroup<T, LambdaConditionGroup<T>, LambdaCondition<T>>
    implements ILambdaCondition<T, LambdaConditionGroup<T>> {

  public static <T> LambdaConditionGroup<T> of(final LambdaCondition<T> lambdaCondition) {
    final LambdaConditionGroup<T> group = new LambdaConditionGroup<>();
    final LambdaCondition<T> condition = new LambdaCondition<>();
    condition.setEntityInfo(lambdaCondition.getEntityInfo());
    condition.setQueryEntityMetaData(lambdaCondition.getQueryEntityMetaData());
    group.condition = condition;
    return group;
  }

  public static <T> LambdaConditionGroup<T> of(final Class<T> clazz) {
    return new LambdaConditionGroup<>(clazz);
  }

  public static <T> LambdaConditionGroup<T> of(final QueryColumn<T> queryColumn) {
    LambdaConditionGroup<T> conditionGroup = new LambdaConditionGroup<>();
    return new LambdaConditionGroup<>(queryColumn);
  }

  public static <T> LambdaConditionGroup<T> of(
      final Class<T> clazz, String tableName, String tableAlia) {
    return new LambdaConditionGroup<>(clazz, tableName, tableAlia);
  }

  private LambdaConditionGroup() {}

  private LambdaConditionGroup(final Class<T> clazz) {
    this.condition = new LambdaCondition<>(clazz, null, null);
  }

  private LambdaConditionGroup(final QueryColumn<T> queryColumn) {
    this.condition = new LambdaCondition<>(queryColumn);
  }

  private LambdaConditionGroup(final Class<T> clazz, String tableName, String tableAlia) {
    this.condition = new LambdaCondition<>(clazz, tableName, tableAlia);
  }

  @Override
  public <V> LambdaConditionGroup<T> eq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.eq(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> ne(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ne(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> gt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.gt(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> ge(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ge(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> lt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.lt(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> le(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.le(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> like(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.like(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.notLike(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> between(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, field, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, field, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> isNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNull(condition, field);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> isNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNotNull(condition, field);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> in(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, field, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> notIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, field, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orEq(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orNe(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGt(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGe(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLt(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLe(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orLike(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orNotLike(condition, field, value);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, field, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, field, start, end);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNull(condition, field);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNotNull(condition, field);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, field, values);
    return this;
  }

  @Override
  public <V> LambdaConditionGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, field, values);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> and(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> or(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  @Override
  public LambdaConditionGroup<T> groupBy(@Nonnull final IFunction<T, ?> field) {
    this.condition.groupBy(field);
    return this;
  }
}
