package io.github.ramerf.wind.core.util;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.springframework.data.domain.*;

import static io.github.ramerf.wind.core.util.CollectionUtils.toList;
import static java.util.stream.Collectors.toCollection;

/**
 * 分页工具类.
 *
 * @author ramer
 * @since 2019 /12/26
 */
@SuppressWarnings("unused")
public class PageUtils {
  /**
   * 转换{@link Page}对象,可选过滤.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param page the page
   * @param mapFunction the map function
   * @return the page
   */
  public static <T, R> Page<R> toPage(
      final Page<T> page, @Nonnull final Function<T, R> mapFunction) {
    return toPage(page, mapFunction, null);
  }

  /**
   * 转换{@link Page}对象,可选过滤.
   *
   * @param <T> the type parameter
   * @param filterFunction the filter function
   * @param page the page
   * @return the page
   */
  public static <T> Page<T> toPage(@Nonnull final Predicate<T> filterFunction, final Page<T> page) {
    return toPage(page, null, filterFunction);
  }

  /**
   * 转换{@link Page}对象,可选过滤.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param page the page
   * @param mapFunction the map function
   * @param filterFunction the filter function
   * @return the page
   */
  @SuppressWarnings("unchecked")
  public static <T, R> Page<R> toPage(
      final Page<T> page, final Function<T, R> mapFunction, final Predicate<R> filterFunction) {
    if (page == null) {
      return new PageImpl<>(Collections.emptyList());
    }
    if (CollectionUtils.isEmpty(page.getContent())) {
      return new PageImpl<>(Collections.emptyList(), page.getPageable(), page.getTotalElements());
    }
    final Stream<R> stream =
        page.getContent().stream().map(o -> mapFunction != null ? mapFunction.apply(o) : (R) o);
    List<R> content =
        Optional.ofNullable(filterFunction)
            .map(stream::filter)
            .orElse(stream)
            .collect(toCollection(LinkedList::new));
    return new PageImpl<>(content, page.getPageable(), page.getTotalElements());
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param <T> the type parameter
   * @param list the list
   * @return the page
   */
  public static <T> Page<T> toPage(final List<T> list) {
    return new PageImpl<>(list);
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param <T> the type parameter
   * @param list the list
   * @param total 总数
   * @return the page
   */
  public static <T> Page<T> toPage(final List<T> list, final long total) {
    return new PageImpl<>(list, PageRequest.of(0, list.size()), total);
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param <T> the type parameter
   * @param list the list
   * @param total 总数
   * @param page 当前页,从0开始
   * @param size 每页大小
   * @return the page
   */
  public static <T> Page<T> toPage(
      final List<T> list, final long total, final int page, final int size) {
    return new PageImpl<>(list, PageRequest.of(page, size), total);
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param <T> the type parameter
   * @param list the list
   * @param total 总数
   * @param page 当前页,从0开始
   * @param size 每页大小
   * @return the page
   */
  public static <T> Page<T> toPage(
      final List<T> list, final long total, final int page, final int size, final Sort sort) {
    return new PageImpl<>(list, PageRequest.of(page, size, sort), total);
  }

  /**
   * 转换{@link Page}对象,可选过滤.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param list the list
   * @param mapFunction the map function
   * @param filterFunction the filter function
   * @return the page
   */
  public static <T, R> Page<R> toPage(
      final List<T> list,
      @Nonnull final Function<T, R> mapFunction,
      final Predicate<R> filterFunction) {
    Objects.requireNonNull(mapFunction);
    return new PageImpl<>(toList(list, mapFunction, filterFunction));
  }
}
