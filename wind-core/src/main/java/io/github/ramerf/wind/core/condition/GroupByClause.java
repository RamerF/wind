package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.LinkedList;
import java.util.List;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;

/**
 * GROUP BY column1,column2.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
public class GroupByClause<T extends AbstractEntity> {
  @Setter @Getter private String tableAlia;
  @Getter private final List<String> cols = new LinkedList<>();

  public GroupByClause<T> col(@Nonnull final IFunction<T, ?> function) {
    return col(function, null);
  }

  public GroupByClause<T> col(@Nonnull final IFunction<T, ?> function, final String columnAlia) {
    cols.add(StringUtils.nonEmpty(columnAlia) ? columnAlia : BeanUtils.methodToColumn(function));
    return this;
  }
}