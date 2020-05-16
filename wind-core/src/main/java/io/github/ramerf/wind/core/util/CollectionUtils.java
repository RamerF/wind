package io.github.ramerf.wind.core.util;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import org.springframework.lang.Nullable;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@SuppressWarnings("unused")
public class CollectionUtils {

  public static boolean isEmpty(@Nullable Collection<?> collection) {
    return (collection == null || collection.isEmpty());
  }

  public static boolean isEmpty(@Nullable Map<?, ?> map) {
    return (map == null || map.isEmpty());
  }

  public static boolean nonEmpty(@Nullable Collection<?> collection) {
    return !isEmpty(collection);
  }

  public static boolean nonEmpty(@Nullable Map<?, ?> map) {
    return !isEmpty(map);
  }

  public static void doIfNonEmpty(
      @Nullable Collection<?> collection, @Nonnull Consumer<Collection<?>> consumer) {
    if (nonEmpty(collection)) {
      consumer.accept(collection);
    }
  }

  /** 转换 {@link List} 对象,可选过滤. */
  public static <T, R> List<R> toList(
      final List<T> list,
      @Nonnull final Function<T, R> mapFunction,
      final Predicate<R> filterFunction) {
    Objects.requireNonNull(mapFunction);
    return Objects.isNull(filterFunction)
        ? list.stream().map(mapFunction).collect(Collectors.toList())
        : list.stream().map(mapFunction).filter(filterFunction).collect(Collectors.toList());
  }
}
