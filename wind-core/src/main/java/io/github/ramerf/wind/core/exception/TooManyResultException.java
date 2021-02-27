package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 未返回期望的结果数量.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class TooManyResultException extends RuntimeException {
  private final int count;

  public TooManyResultException(final int count) {
    this.count = count;
  }

  @Override
  public String toString() {
    return "Unexpect result,Expect one but get " + count;
  }
}
