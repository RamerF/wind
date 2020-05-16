package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.LinkedList;
import java.util.List;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Order;

/**
 * The type Sort column.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@NoArgsConstructor(staticName = "of")
public class SortColumn {
  private final Sort sort = Sort.unsorted();
  /** The Orders. */
  List<Order> orders = new LinkedList<>();

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
    return asc(EntityHelper.getColumn(function));
  }

  /**
   * {@link SortColumn#asc(IFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public SortColumn asc(final String column) {
    orders.add(Order.asc(column));
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
    return desc(EntityHelper.getColumn(function));
  }

  /**
   * {@link SortColumn#desc(IFunction)}
   *
   * @param column the column
   * @return the sort column
   */
  public SortColumn desc(final String column) {
    orders.add(Order.desc(column));
    return this;
  }
}
