package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.function.GetterFunction;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Getter;
import org.springframework.data.domain.*;
import org.springframework.data.domain.Sort.Order;

/**
 * 排序规则.示例: <code>Pages.of(1, 10).asc(Foo::getUpdateTime)</code>
 *
 * @author ramer
 * @since 2020 /1/5
 */
public class Pages {
  private final Sort sort = Sort.unsorted();
  @Getter private int page;
  @Getter private int size;
  /** The Orders. */
  List<Order> orders = new LinkedList<>();

  public static final Pages UNPAGED = new Pages();

  private Pages() {}

  /** size小于1时抛出{@link SimpleException}异常 */
  public static <T> Pages of(final int size) {
    return of(1, size, Collections.emptyList());
  }

  /**
   *
   * <li>当且仅当page和size为0时,返回null.
   * <li>page或size小于1时抛出{@link SimpleException}异常
   */
  public static <T> Pages of(final int page, final int size) {
    return of(page, size, Collections.emptyList());
  }

  public static <T> Pages of(final int page, final int size, @Nonnull final List<Order> orders) {
    if (page == 0 && size == 0) {
      return null;
    }
    if (page < 1 || size < 1) {
      throw new SimpleException("page,size不能小于1");
    }
    final Pages pages = new Pages();
    pages.page = page;
    pages.size = size;
    pages.orders = orders;
    return pages;
  }

  public <T> Pages asc(final GetterFunction<T, ?> getter) {
    return asc(getter.getColumn());
  }

  /**
   * {@link Pages#asc(GetterFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public Pages asc(final String column) {
    orders.add(Sort.Order.asc(column));
    return this;
  }

  public <T> Pages desc(final GetterFunction<T, ?> getter) {
    return desc(getter.getColumn());
  }

  /**
   * {@link Pages#desc(GetterFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public Pages desc(final String column) {
    orders.add(Sort.Order.desc(column));
    return this;
  }

  public Pageable getPageable() {
    if (page == 0 && size == 0) {
      return null;
    }
    return PageRequest.of(page - 1, size, orders.isEmpty() ? Sort.unsorted() : Sort.by(orders));
  }
}
