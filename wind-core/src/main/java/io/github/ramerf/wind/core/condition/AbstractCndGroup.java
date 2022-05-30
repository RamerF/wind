package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.domain.Pageable;
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
 * @author ramer
 */
public abstract class AbstractCndGroup<
        POJO,
        CONDITION_GROUP extends AbstractCndGroup<POJO, CONDITION_GROUP, CONDITION>,
        CONDITION extends AbstractCnd<POJO, CONDITION>>
    implements Condition<POJO, CONDITION_GROUP> {
  @Getter protected CONDITION condition;

  @Override
  public Class<POJO> getClazz() {
    return condition.getClazz();
  }

  @Nonnull
  @Override
  public Pageable getPageRequest() {
    return condition.getPageRequest();
  }

  @Override
  public String getString() {
    return condition.getString();
  }

  @Override
  public String toString() {
    return getString();
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
  public CONDITION_GROUP and(final boolean cond, final String sql) {
    condition.and(cond, sql);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public CONDITION_GROUP or(final boolean cond, final String sql) {
    condition.or(cond, sql);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public final CONDITION_GROUP appendLogicNotDelete() {
    condition.appendLogicNotDelete();
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }

  @Override
  public CONDITION_GROUP logicDelete(final boolean deleted) {
    condition.logicDelete(deleted);
    //noinspection unchecked
    return (CONDITION_GROUP) this;
  }
}
