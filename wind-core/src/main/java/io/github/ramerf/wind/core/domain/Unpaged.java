package io.github.ramerf.wind.core.domain;

import io.github.ramerf.wind.core.domain.Sort.Order;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

/** 无分页信息. */
public class Unpaged implements Pageable {

  /** 排序规则. */
  private final Sort sort;

  public static final Unpaged INSTANCE = new Unpaged(Collections.emptyList());

  private Unpaged(@Nonnull final List<Order> orders) {
    this.sort = Sort.by(orders);
  }

  public static Unpaged of(@Nonnull final List<Order> orders) {
    return new Unpaged(orders);
  }

  @Override
  public boolean isPaged() {
    return false;
  }

  @Override
  public Pageable next() {
    return this;
  }

  @Override
  public Pageable previous() {
    return this;
  }

  @Override
  public Sort getSort() {
    return sort;
  }

  @Override
  public int getPageSize() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getPageNumber() {
    throw new UnsupportedOperationException();
  }

  @Override
  public long getOffset() {
    throw new UnsupportedOperationException();
  }
}
