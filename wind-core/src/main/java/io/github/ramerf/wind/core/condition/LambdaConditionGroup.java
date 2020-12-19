package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.function.IFunction;
import java.util.Collection;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * @author ramer
 * @since 24/11/2020
 */
public class LambdaConditionGroup<T extends AbstractEntityPoJo<T, ?>> {
  @Getter private LambdaCondition<T> condition;

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaConditionGroup<T> getInstance(
      final LambdaCondition<T> lambdaCondition) {
    final LambdaConditionGroup<T> group = new LambdaConditionGroup<>();
    final LambdaCondition<T> condition = new LambdaCondition<>();
    condition.setEntityInfo(lambdaCondition.getEntityInfo());
    condition.setQueryEntityMetaData(lambdaCondition.getQueryEntityMetaData());
    group.condition = condition;
    return group;
  }

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaConditionGroup<T> getInstance(
      final Class<T> clazz) {
    return new LambdaConditionGroup<>(clazz);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaConditionGroup<T> getInstance(
      final QueryColumn<T> queryColumn) {
    LambdaConditionGroup<T> conditionGroup = new LambdaConditionGroup<>();
    return new LambdaConditionGroup<>(queryColumn);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> LambdaConditionGroup<T> getInstance(
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

  public <V> LambdaConditionGroup<T> andEq(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.eq(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.eq(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ne(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ne(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andGt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.gt(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.gt(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andGe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ge(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.ge(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.lt(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.lt(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.le(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.le(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.like(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.like(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.notLike(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.notLike(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> andBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.between(true, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> andBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.between(condition, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.notBetween(true, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.notBetween(condition, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIsNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.isNull(true, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNull(condition, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIsNotNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.isNotNull(true, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.isNotNull(condition, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.in(true, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> andIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.in(condition, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.notIn(true, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> andNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.notIn(condition, field, values);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>>
      LambdaConditionGroup<T> andEq(
          @Nonnull final IFunction<T, ?> field,
          @Nonnull final QueryColumn<Q> queryColumn,
          @Nonnull final IFunction<R, ?> field2) {
    this.condition.eq(true, field, queryColumn, field2);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>>
      LambdaConditionGroup<T> andEq(
          final boolean condition,
          @Nonnull final IFunction<T, ?> field,
          @Nonnull final AbstractQueryEntity<Q> queryColumn,
          @Nonnull final IFunction<R, ?> field2) {
    this.condition.eq(condition, field, queryColumn, field2);
    return this;
  }

  public LambdaConditionGroup<T> andExists(@Nonnull final Condition<T> childConditions) {
    this.condition.exists(true, childConditions);
    return this;
  }

  public LambdaConditionGroup<T> andExists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    this.condition.exists(condition, childConditions);
    return this;
  }

  public <V> LambdaConditionGroup<T> orEq(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orEq(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orEq(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orEq(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orNe(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orNe(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orGt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGt(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orGt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGt(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orGe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGe(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orGe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orGe(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLt(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLt(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLt(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLt(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLe(@Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLe(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLe(
      final boolean condition, @Nonnull final IConsumer<T, V> field, final V value) {
    this.condition.orLe(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orLike(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orLike(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotLike(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orNotLike(true, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotLike(
      final boolean condition, @Nonnull final IConsumer<T, V> field, @Nonnull final V value) {
    this.condition.orNotLike(condition, field, value);
    return this;
  }

  public <V> LambdaConditionGroup<T> orBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.orBetween(true, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> orBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orBetween(condition, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotBetween(
      @Nonnull final IConsumer<T, V> field, @Nonnull final V start, @Nonnull final V end) {
    this.condition.orNotBetween(true, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotBetween(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final V start,
      @Nonnull final V end) {
    this.condition.orNotBetween(condition, field, start, end);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIsNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNull(true, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIsNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNull(condition, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIsNotNull(@Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNotNull(true, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIsNotNull(
      final boolean condition, @Nonnull final IConsumer<T, V> field) {
    this.condition.orIsNotNull(condition, field);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.orIn(true, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> orIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orIn(condition, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotIn(
      @Nonnull final IConsumer<T, V> field, @Nonnull final Collection<V> values) {
    this.condition.orNotIn(true, field, values);
    return this;
  }

  public <V> LambdaConditionGroup<T> orNotIn(
      final boolean condition,
      @Nonnull final IConsumer<T, V> field,
      @Nonnull final Collection<V> values) {
    this.condition.orNotIn(condition, field, values);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>>
      LambdaConditionGroup<T> orEq(
          @Nonnull final IFunction<T, ?> field,
          @Nonnull final QueryColumn<Q> queryColumn,
          @Nonnull final IFunction<R, ?> field2) {
    this.condition.orEq(true, field, queryColumn, field2);
    return this;
  }

  public <R extends AbstractEntity, Q extends AbstractEntityPoJo<Q, ?>>
      LambdaConditionGroup<T> orEq(
          final boolean condition,
          @Nonnull final IFunction<T, ?> field,
          @Nonnull final AbstractQueryEntity<Q> queryColumn,
          @Nonnull final IFunction<R, ?> field2) {
    this.condition.orEq(condition, field, queryColumn, field2);
    return this;
  }

  public LambdaConditionGroup<T> orExists(@Nonnull final Condition<T> childConditions) {
    this.condition.orExists(true, childConditions);
    return this;
  }

  public LambdaConditionGroup<T> orExists(
      final boolean condition, @Nonnull final Condition<T> childConditions) {
    this.condition.orExists(condition, childConditions);
    return this;
  }

  public LambdaConditionGroup<T> and(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.and(group);
    return this;
  }

  public LambdaConditionGroup<T> and(@Nonnull Consumer<LambdaConditionGroup<T>> group) {
    this.condition.and(group);
    return this;
  }

  public LambdaConditionGroup<T> and(final String sql) {
    this.condition.and(sql);
    return this;
  }

  public LambdaConditionGroup<T> or(@Nonnull LambdaConditionGroup<T> group) {
    this.condition.or(group);
    return this;
  }

  public LambdaConditionGroup<T> or(@Nonnull Consumer<LambdaConditionGroup<T>> group) {
    this.condition.or(group);
    return this;
  }

  public LambdaConditionGroup<T> or(final String sql) {
    this.condition.or(sql);
    return this;
  }

  public boolean isEmpty() {
    return this.condition.valueTypes.size() <= 0;
  }
}
