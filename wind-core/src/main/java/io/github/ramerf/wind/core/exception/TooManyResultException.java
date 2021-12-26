package io.github.ramerf.wind.core.exception;

import io.github.ramerf.wind.core.executor.DataAccessException;

/**
 * 未返回期望的结果数量.
 *
 * @since 2021.01.31
 * @author ramer
 */
public class TooManyResultException extends DataAccessException {
  private final int count;

  public TooManyResultException(final int count) {
    super("Too many result exception");
    this.count = count;
  }

  @Override
  public String toString() {
    return "Unexpect result,Expect one but get " + count;
  }
}
