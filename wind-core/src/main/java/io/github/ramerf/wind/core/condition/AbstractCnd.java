package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.function.IFunction;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.domain.Sort.Order;

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
  protected int page;
  protected int size;
  private final List<Order> orders = new LinkedList<>();

  /**
   * 限制获取记录数.<br>
   *
   * @throws SimpleException 当size小于1时
   */
  public CND limit(final int size) throws SimpleException {
    return limit(1, size);
  }

  /**
   * 分页参数.<br>
   * <li>当page,size同时为0时,表示不分页
   * <li>当page或size小于1时,抛出{@link SimpleException}异常
   *
   * @param page 从1开始
   * @param size the size
   * @return the u
   * @throws SimpleException 当page或size小于1且不同时为0时
   */
  public CND limit(final int page, final int size) throws SimpleException {
    if (page == 0 && size == 0) {
      //noinspection unchecked
      return (CND) this;
    }
    if (page < 1 || size < 1) {
      throw SimpleException.of("page,size不能小于1");
    }
    this.page = page;
    this.size = size;
    //noinspection unchecked
    return (CND) this;
  }

  /** 默认倒序 desc. */
  public CND orderBy(@Nonnull final IFunction<POJO, ?> field) {
    orders.add(Order.desc(field.getColumn()));
    //noinspection unchecked
    return (CND) this;
  }

  public CND orderBy(@Nonnull final IFunction<POJO, ?> field, @Nonnull final Direction direction) {
    orders.add(
        direction.isAscending() ? Order.asc(field.getColumn()) : Order.desc(field.getColumn()));
    //noinspection unchecked
    return (CND) this;
  }

  @Override
  @Nullable
  public Pages getPages() {
    return Pages.of(page, size, orders);
  }

  @Override
  public QueryEntityMetaData<POJO> getQueryEntityMetaData() {
    return getCondition().getQueryEntityMetaData();
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
