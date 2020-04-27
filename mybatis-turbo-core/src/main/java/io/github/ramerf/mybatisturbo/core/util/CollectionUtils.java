package io.github.ramerf.mybatisturbo.core.util;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import org.springframework.beans.BeanUtils;
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

  /** 转换{@link Page}对象,可选过滤. */
  public static <T extends AbstractEntity, R> Page<R> toPage(
      final Page<T> page, @Nonnull final Function<T, R> mapFunction) {
    return toPage(page, mapFunction, null);
  }

  /** 转换{@link Page}对象,可选过滤. */
  public static <T extends AbstractEntity> Page<T> toPage(
      @Nonnull final Predicate<T> filterFunction, final Page<T> page) {
    return toPage(page, null, filterFunction);
  }

  /** 转换{@link Page}对象,可选过滤. */
  @SuppressWarnings("unchecked")
  public static <T extends AbstractEntity, R> Page<R> toPage(
      final Page<T> page, final Function<T, R> mapFunction, final Predicate<R> filterFunction) {
    Page<R> result = new Page<>();
    if (Objects.isNull(page) || CollectionUtils.isEmpty(page.getRecords())) {
      return new Page<>();
    }
    BeanUtils.copyProperties(page, result);
    if (Objects.nonNull(mapFunction)) {
      final Stream<R> stream = page.getRecords().stream().map(mapFunction);
      result.setRecords(
          Optional.ofNullable(filterFunction)
              .map(stream::filter)
              .orElse(stream)
              .collect(Collectors.toList()));
    } else {
      final Stream<T> stream = page.getRecords().stream();
      result.setRecords(
          (List<R>)
              Optional.ofNullable(filterFunction)
                  .map(filterFunc -> stream.filter(t -> filterFunc.test((R) t)))
                  .orElse(stream)
                  .collect(Collectors.toList()));
    }
    return result;
  }

  /** 将 {@link List} 转换为 {@link Page}对象. */
  public static <T extends AbstractEntity> Page<T> toPage(final List<T> list) {
    Page<T> page = new Page<>();
    page.setRecords(list);
    page.setTotal(list.size());
    return page;
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param total 总数
   */
  public static <T extends AbstractEntity> Page<T> toPage(final List<T> list, final long total) {
    Page<T> pages = new Page<>();
    pages.setRecords(list);
    pages.setTotal(total);
    return pages;
  }

  /**
   * 将 {@link List} 转换为 {@link Page}对象.
   *
   * @param total 总数
   * @param page 当前页,从1开始
   * @param size 每页大小
   */
  public static <T> Page<T> toPage(
      final List<T> list, final long total, final int page, final int size) {
    Page<T> pages = new Page<>();
    pages.setRecords(list);
    pages.setTotal(total);
    pages.setCurrent(page);
    pages.setSize(size);
    return pages;
  }

  /** 转换{@link Page}对象,可选过滤. */
  public static <T, R> Page<R> toPage(
      final List<T> list, final Function<T, R> mapFunction, final Predicate<R> filterFunction) {
    Objects.requireNonNull(mapFunction);
    Page<R> page = new Page<>();
    List<R> rs = toList(list, mapFunction, null);
    if (Objects.nonNull(filterFunction)) {
      rs = rs.stream().filter(filterFunction).collect(Collectors.toList());
    }
    page.setRecords(rs);
    return page;
  }
}
