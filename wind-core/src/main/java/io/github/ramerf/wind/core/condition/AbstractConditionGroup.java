package io.github.ramerf.wind.core.condition;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * The type Abstract condition group.
 *
 * @param <POJO> pojo
 * @param <CONDITION_GROUP> 当前对象
 * @param <CONDITION> 当前对象持有的{@link Condition}对象
 * @since 2021.08.15
 * @author ramer Xiaofeng
 */
public abstract class AbstractConditionGroup<
        POJO,
        CONDITION_GROUP extends AbstractConditionGroup<POJO, CONDITION_GROUP, CONDITION>,
        CONDITION extends AbstractCondition<POJO, CONDITION>>
    implements Condition<POJO, CONDITION_GROUP> {
  @Getter protected CONDITION condition;

  @Override
  public QueryEntityMetaData<POJO> getQueryEntityMetaData() {
    return condition.getQueryEntityMetaData();
  }

  @Override
  public String getString() {
    return condition.getString();
  }

  @Override
  public List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex) {
    return condition.getValues(startIndex);
  }

  @Override
  public List<Object> getOriginValues() {
    return condition.getOriginValues();
  }

  @Override
  public boolean isEmpty() {
    return condition.isEmpty();
  }

  @Override
  public CONDITION_GROUP eq(
      final boolean condition, @Nonnull final Field field, final Object value) {
    this.condition.eq(condition, field, value);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public CONDITION_GROUP in(
      final boolean condition, @Nonnull final Field field, @Nonnull final Collection<?> values) {
    this.condition.in(condition, field, values);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public CONDITION_GROUP and(final String sql) {
    condition.and(sql);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public CONDITION_GROUP or(final String sql) {
    condition.or(sql);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public final CONDITION_GROUP appendLogicNotDelete() {
    condition.appendLogicNotDelete();
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }
}
