package io.github.ramerf.wind.core.domain;

/**
 * 分页信息.
 *
 * @author ramer
 * @since 2022.03.05
 */
public interface Pageable {
  /** 返回一个无分页信息的实例. */
  static Pageable unpaged() {
    return Unpaged.INSTANCE;
  }

  /** 是否包含分页信息. */
  default boolean isPaged() {
    return true;
  }

  /** 当前页号,首页为1. */
  int getPageNumber();

  /** 每页记录数. */
  int getPageSize();

  /** 当前页的起始位置. */
  long getOffset();

  Pageable next();

  Pageable previous();

  Sort getSort();
}