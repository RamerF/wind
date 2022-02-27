package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.domain.Sort;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.domain.Sort.Order;
import io.github.ramerf.wind.core.exception.SimpleException;
import io.github.ramerf.wind.core.function.GetterFunction;
import java.util.Collections;
import java.util.List;
import javax.annotation.Nonnull;

/**
 * 排序规则.示例: <code>Pages.of(1, 10).asc(Foo::getUpdateTime)</code>
 *
 * @author ramer
 * @since 2020 /1/5
 */
public class PageRequest {
  private Sort sort = Sort.unsorted();
  private int page;
  private int size;
  /** The Orders. */
  // List<Order> orders = new LinkedList<>();

  public static final PageRequest UNPAGED = new PageRequest();

  private PageRequest() {}

  /** size小于1时抛出{@link SimpleException}异常 */
  public static <T> PageRequest of(final int size) {
    return of(1, size, Collections.emptyList());
  }

  /**
   *
   * <li>当且仅当page和size为0时,返回null.
   * <li>page或size小于1时抛出{@link SimpleException}异常
   */
  public static <T> PageRequest of(final int page, final int size) {
    return of(page, size, Collections.emptyList());
  }

  public static <T> PageRequest of(
      final int page, final int size, @Nonnull final List<Order> orders) {
    return of(page, size, Sort.by(orders));
  }

  public static <T> PageRequest of(final int page, final int size, Sort sort) {
    if (page == 0 && size == 0) {
      return null;
    }
    if (page < 1 || size < 1) {
      throw new SimpleException("page,size不能小于1");
    }
    final PageRequest pageRequest = new PageRequest();
    pageRequest.page = page;
    pageRequest.size = size;
    pageRequest.sort = sort;
    return pageRequest;
  }

  public <T> PageRequest asc(final GetterFunction<T, ?> getter) {
    return asc(getter.getColumn());
  }

  /**
   * {@link PageRequest#asc(GetterFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public PageRequest asc(final String column) {
    sort.and(Sort.by(Direction.ASC, column));
    return this;
  }

  public <T> PageRequest desc(final GetterFunction<T, ?> getter) {
    return desc(getter.getColumn());
  }

  /**
   * {@link PageRequest#desc(GetterFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public PageRequest desc(final String column) {
    sort.and(Sort.by(Direction.DESC, column));
    return this;
  }

  public int getPage() {
    return page;
  }

  public int getSize() {
    return size;
  }

  public long getOffset() {
    return (long) page * (long) size;
  }

  public Sort getSort() {
    return sort;
  }
}
