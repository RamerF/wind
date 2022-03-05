package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.domain.PageRequest;
import io.github.ramerf.wind.core.domain.Pageable;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.domain.Sort.Order;
import io.github.ramerf.wind.core.function.GetterFunction;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;

/**
 * The type Abstract cnd.
 *
 * @param <POJO> pojo对象
 * @param <CND> 当前对象
 * @param <CONDITION> 持有的Condition对象,比如:LambdaCondition,StringCondition
 * @since 2021.08.22
 * @author ramer
 */
public abstract class AbstractCnd<
        POJO, //
        CND extends AbstractCnd<POJO, CND, CONDITION>,
        CONDITION extends Condition<POJO, CONDITION>>
    implements Cnd<POJO, CND, CONDITION>, Condition<POJO, CND> {
  /** 从1开始 */
  protected int page;

  protected int size;

  private final List<Order> orders = new LinkedList<>();
  protected final Class<POJO> clazz;

  protected AbstractCnd(final Class<POJO> clazz) {
    this.clazz = clazz;
  }

  @Nonnull
  @Override
  public Class<POJO> getClazz() {
    return clazz;
  }

  /** 从第一页开始,限制获取记录数. */
  public CND limit(final int size) {
    return limit(1, size);
  }

  /** 分页参数.page 从1开始 */
  public CND limit(final int page, final int size) {
    if (page < 1 || size < 1) {
      throw new IllegalArgumentException("page,size不能小于1");
    }
    this.page = page;
    this.size = size;
    //noinspection unchecked
    return (CND) this;
  }

  /** 默认倒序 desc. */
  public CND orderBy(@Nonnull final GetterFunction<POJO, ?> getter) {
    orders.add(Order.desc(getter.getColumn()));
    //noinspection unchecked
    return (CND) this;
  }

  public CND orderBy(
      @Nonnull final GetterFunction<POJO, ?> getter, @Nonnull final Direction direction) {
    orders.add(
        direction.isAscending() ? Order.asc(getter.getColumn()) : Order.desc(getter.getColumn()));
    //noinspection unchecked
    return (CND) this;
  }

  @Nonnull
  @Override
  public Pageable getPageRequest() {
    return page > 0 && size > 0 ? PageRequest.of(page, size, orders) : Pageable.unpaged();
  }

  @Override
  public String getString() {
    return getCondition().getString();
  }

  @Override
  public String toString() {
    return getString();
  }

  @Override
  public List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex) {
    return getCondition().getValues(startIndex);
  }

  @Override
  public List<Object> getOriginValues() {
    return getCondition().getOriginValues();
  }

  @Override
  public boolean isEmpty() {
    return getCondition().isEmpty();
  }

  @Override
  public CND eq(final boolean condition, @Nonnull final Field field, final Object value) {
    getCondition().eq(condition, field, value);
    //noinspection unchecked
    return (CND) this;
  }

  @Override
  public CND in(
      final boolean condition, @Nonnull final Field field, @Nonnull final Collection<?> values) {
    getCondition().in(condition, field, values);
    //noinspection unchecked
    return (CND) this;
  }

  @Override
  public CND and(final boolean condition, final String sql) {
    getCondition().and(condition, sql);
    //noinspection unchecked
    return (CND) this;
  }

  @Override
  public CND or(final boolean condition, final String sql) {
    getCondition().or(condition, sql);
    //noinspection unchecked
    return (CND) this;
  }

  @Override
  public final CND appendLogicNotDelete() {
    getCondition().appendLogicNotDelete();
    //noinspection unchecked
    return (CND) this;
  }
}
