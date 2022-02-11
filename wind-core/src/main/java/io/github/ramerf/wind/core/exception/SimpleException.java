package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 简单异常,不记录调用栈.
 *
 * @since 2020.12.22
 * @author ramer
 */
@Slf4j
public class SimpleException extends CommonException {

  public SimpleException(final String message) {
    super(message);
  }

  public SimpleException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public SimpleException(final Throwable cause) {
    super(cause);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return this;
  }
}
