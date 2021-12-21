package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.function.GetterFunction;
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
  @Getter private GetterFunction<T, ?> getter;
  @Getter private Direction direction;

  public static <T> OrderBy<T> of(@Nonnull GetterFunction<T, ?> getter) {
    return of(getter, Direction.ASC);
  }

  public static <T> OrderBy<T> of(
      @Nonnull final GetterFunction<T, ?> getter, @Nonnull final Direction direction) {
    OrderBy<T> orderBy = new OrderBy<>();
    orderBy.getter = getter;
    orderBy.direction = direction;
    return orderBy;
  }
}
