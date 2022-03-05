package io.github.ramerf.wind.core.domain;

/** 无分页信息. */
public enum Unpaged implements Pageable {
  INSTANCE;

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
    return Sort.unsorted();
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
