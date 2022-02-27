package io.github.ramerf.wind.core.domain;

import java.util.List;

/**
 * 分页对象.
 *
 * @author ramer
 * @since 2022.02.27
 */
public class Page<T> {
  /** 页号,从0开始 */
  private int page;
  /** 每页数量. */
  private int size;
  /** 总记录数. */
  private long total;
  /** 当前页列表数据. */
  private List<T> content;
  /** 排序规则. */
  private Sort sort = Sort.unsorted();

  public Page() {}

  public Page(final List<T> content) {
    this.content = content;
    this.total = null == content ? 0 : content.size();
    this.page = 0;
    this.size = (int) this.total;
  }

  public Page(final int page, final int size, final int total, final List<T> content) {
    this(page, size, total, content, Sort.unsorted());
  }

  public Page(final int page, final int size, final long total, final List<T> content, Sort sort) {
    if (page < 0) {
      throw new IllegalArgumentException("Page number must not be less than zero!");
    }
    if (size < 1) {
      throw new IllegalArgumentException("Page size must not be less than one!");
    }
    this.page = page;
    this.size = size;
    this.total = total;
    this.content = content;
    this.sort = sort;
  }
}
