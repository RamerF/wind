package io.github.ramerf.wind.core.domain;

import io.github.ramerf.wind.core.util.Asserts;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * 分页对象.
 *
 * @author ramer
 * @since 2022.02.27
 */
public class Page<T> {
  private Pageable pageable;
  private final List<T> content = new ArrayList<>();

  @SuppressWarnings("rawtypes")
  public static final Page EMPTY = Page.of(Collections.emptyList());

  /** 总记录数. */
  private long total;

  private Page() {}

  public static <T> Page<T> of(@Nonnull final List<T> content) {
    return of(content, Pageable.unpaged());
  }

  public static <T> Page<T> of(@Nonnull final List<T> content, final Pageable pageable) {
    return of(content, pageable, 0);
  }

  public static <T> Page<T> of(
      @Nonnull final List<T> content, @Nonnull final Pageable pageable, final long total) {
    Asserts.notNull(content, "Content must not be null!");
    Asserts.notNull(pageable, "Pageable must not be null!");
    Page<T> page = new Page<>();
    page.content.addAll(content);
    page.pageable = pageable;
    page.total =
        pageable.isPaged()
            ? Optional.of(pageable)
                .filter(it -> !content.isEmpty()) //
                .filter(it -> it.getOffset() + it.getPageSize() > total) //
                .map(it -> it.getOffset() + content.size()) //
                .orElse(total)
            : total;
    return page;
  }

  /** 总页数. */
  public int getTotalPages() {
    return getSize() == 0 ? 1 : (int) Math.ceil((double) total / (double) getSize());
  }

  /** 总记录数. */
  public long getTotalElements() {
    return total;
  }

  /** 每页数量. */
  public int getSize() {
    return pageable.isPaged() ? pageable.getPageSize() : 0;
  }

  /** 当前页号,首页为1. */
  public int getPageNumber() {
    return pageable.isPaged() ? pageable.getPageNumber() : 1;
  }

  /** 当前页记录数. */
  public int getNumberOfElements() {
    return content.size();
  }

  /** 当前页记录列表. */
  public List<T> getContent() {
    return content;
  }

  /** 排序规则. */
  public Sort getSort() {
    return pageable.getSort();
  }

  /** 是否还有上一页. */
  public boolean hasPrevious() {
    return getPageNumber() > 1;
  }

  /** 是否还有下一页. */
  public boolean hasNext() {
    return getPageNumber() < getTotalPages();
  }

  /** 是否最后一页. */
  public boolean isLast() {
    return !hasNext();
  }

  public Pageable getPageable() {
    return pageable;
  }

  public void setPageable(final Pageable pageable) {
    this.pageable = pageable;
  }

  public void setTotal(final long total) {
    this.total = total;
  }

  @SuppressWarnings("unchecked")
  public static <T> Page<T> empty() {
    return (Page<T>) EMPTY;
  }
}
