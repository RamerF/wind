package io.github.ramerf.wind.core.util;

import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import org.springframework.beans.BeanUtils;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@SuppressWarnings("unused")
public class PageUtils {

  /** 转换{@link Page}对象,可选过滤. */
  public static <T extends AbstractEntity, R> Page<R> toPage(
      final Page<T> page,
      @Nonnull final Function<T, R> mapFunction,
      final Predicate<R> filterFunction) {
    Objects.requireNonNull(mapFunction);
    Page<R> result = new Page<>();
    if (Objects.isNull(page) || CollectionUtils.isEmpty(page.getRecords())) {
      return new Page<>();
    }
    BeanUtils.copyProperties(page, result);
    if (Objects.isNull(filterFunction)) {
      result.setRecords(page.getRecords().stream().map(mapFunction).collect(Collectors.toList()));
    } else {
      result.setRecords(
          page.getRecords().stream()
              .map(mapFunction)
              .filter(filterFunction)
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
  public static <T extends AbstractEntity> Page<T> toPage(
      final List<T> list, final long total, final int page, final int size) {
    Page<T> pages = new Page<>();
    pages.setRecords(list);
    pages.setTotal(total);
    pages.setCurrent(page);
    pages.setSize(size);
    return pages;
  }

  /** 转换{@link Page}对象,可选过滤. */
  public static <T extends AbstractEntity, R> Page<R> toPage(
      final List<T> list, final Function<T, R> mapFunction, final Predicate<R> filterFunction) {
    Objects.requireNonNull(mapFunction);
    Page<R> page = new Page<>();
    List<R> rs = CollectionUtils.toList(list, mapFunction, null);
    if (Objects.nonNull(filterFunction)) {
      rs = rs.stream().filter(filterFunction).collect(Collectors.toList());
    }
    page.setRecords(rs);
    return page;
  }
}
