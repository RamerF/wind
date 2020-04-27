package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.util.BeanUtils;
import java.util.*;
import lombok.Getter;
import lombok.Setter;

/**
 * @see SortColumnBuilder
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
@Getter
@Setter
public class SortColumn {
  private boolean isAsc = Boolean.TRUE;
  private String column;

  private SortColumn() {}

  public static SortColumnBuilder builder() {
    return new SortColumnBuilder();
  }

  public static class SortColumnBuilder {

    private final List<SortColumn> list = new LinkedList<>();

    public <T extends AbstractEntity> SortColumnBuilder desc(final IFunction<T, ?> function) {
      return desc(true, function);
    }

    public <T extends AbstractEntity> SortColumnBuilder desc(
        final boolean condition, final IFunction<T, ?> function) {
      if (condition) {
        list.add(SortColumn.desc(function));
      }
      return this;
    }

    public <T extends AbstractEntity> SortColumnBuilder asc(final IFunction<T, ?> function) {
      return asc(true, function);
    }

    public <T extends AbstractEntity> SortColumnBuilder asc(
        final boolean condition, final IFunction<T, ?> function) {
      if (condition) {
        list.add(SortColumn.asc(function));
      }
      return this;
    }

    /**
     * use {@link SortColumnBuilder#asc(IFunction)} /{@link SortColumnBuilder#desc(IFunction)}
     * instead.
     *
     * @param sortColumn the sortColumn
     * @return sort column builder
     */
    @Deprecated
    public SortColumnBuilder add(final SortColumn sortColumn) {
      list.add(sortColumn);
      return this;
    }

    /**
     * use {@link SortColumnBuilder#asc(boolean,IFunction)} /{@link
     * SortColumnBuilder#desc(boolean,IFunction)} instead.
     *
     * @param condition the condition
     * @param sortColumn the sortColumn
     * @return SortColumnBuilder
     */
    @Deprecated
    public SortColumnBuilder add(final boolean condition, final SortColumn sortColumn) {
      if (condition) {
        list.add(sortColumn);
      }
      return this;
    }

    public List<SortColumn> build() {
      return list;
    }
  }

  public static <T extends AbstractEntity> SortColumn asc(final IFunction<T, ?> function) {
    return asc(BeanUtils.methodToColumn(function));
  }

  /** {@link SortColumn#asc(IFunction)} */
  public static SortColumn asc(final String column) {
    SortColumn sortColumn = new SortColumn();
    sortColumn.setColumn(column);
    return sortColumn;
  }

  public static <T extends AbstractEntity> SortColumn desc(final IFunction<T, ?> function) {
    return desc(BeanUtils.methodToColumn(function));
  }

  /** {@link SortColumn#desc(IFunction)} */
  public static SortColumn desc(final String column) {
    SortColumn sortColumn = new SortColumn();
    sortColumn.setColumn(column);
    sortColumn.setAsc(Boolean.FALSE);
    return sortColumn;
  }
}
