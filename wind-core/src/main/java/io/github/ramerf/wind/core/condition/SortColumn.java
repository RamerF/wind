package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.LinkedList;
import java.util.List;
import javax.annotation.Nonnull;
import org.springframework.data.domain.Sort;

/**
 * 排序规则.示例: <code>SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)</code>
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
public class SortColumn {
  private final Sort sort = Sort.unsorted();
  /** The Orders. */
  List<Sort.Order> orders = new LinkedList<>();

  public static <T extends AbstractEntityPoJo> SortColumn by(
      @Nonnull final IFunction<T, ?> function, Order order) {
    SortColumn sortColumn = new SortColumn();
    if (order.equals(Order.ASC)) {
      sortColumn.asc(function);
    } else {
      sortColumn.desc(function);
    }
    return sortColumn;
  }

  /**
   * Gets sort.
   *
   * @return the sort
   */
  public Sort getSort() {
    return CollectionUtils.isEmpty(orders) ? sort : Sort.by(orders);
  }

  /**
   * Asc sort column.
   *
   * @param <T> the type parameter
   * @param function the function
   * @return the sort column
   */
  public <T extends AbstractEntity> SortColumn asc(final IFunction<T, ?> function) {
    return asc(function.getColumn());
  }

  /**
   * {@link SortColumn#asc(IFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public SortColumn asc(final String column) {
    orders.add(Sort.Order.asc(column));
    return this;
  }

  /**
   * Desc sort column.
   *
   * @param <T> the type parameter
   * @param function the function
   * @return the sort column
   */
  public <T extends AbstractEntity> SortColumn desc(final IFunction<T, ?> function) {
    return desc(function.getColumn());
  }

  /**
   * {@link SortColumn#desc(IFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public SortColumn desc(final String column) {
    orders.add(Sort.Order.desc(column));
    return this;
  }

  public enum Order {
    /** 排序规则. */
    ASC,
    DESC
  }
}
