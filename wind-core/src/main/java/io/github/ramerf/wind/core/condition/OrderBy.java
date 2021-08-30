package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.IFunction;
import javax.annotation.Nonnull;
import lombok.*;
import org.springframework.data.domain.Sort.Direction;

/**
 * 排序规则.示例: <code>OrderBy.of(Foo::getCreateTime, Direction.DESC).asc(Foo::getUpdateTime)</code>
 *
 * @author ramer
 * @since 2020 /1/5
 */
@NoArgsConstructor(access = AccessLevel.NONE)
public class OrderBy<T> {
  @Getter private IFunction<T, ?> field;
  @Getter private Direction direction;

  public static <T> OrderBy<T> of(@Nonnull IFunction<T, ?> field) {
    return of(field, Direction.ASC);
  }

  public static <T> OrderBy<T> of(
      @Nonnull final IFunction<T, ?> field, @Nonnull final Direction direction) {
    OrderBy<T> orderBy = new OrderBy<>();
    orderBy.field = field;
    orderBy.direction = direction;
    return orderBy;
  }
}
