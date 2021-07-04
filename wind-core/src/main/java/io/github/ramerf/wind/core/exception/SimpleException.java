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

  public static SimpleException of() {
    return new SimpleException();
  }

  public static SimpleException of(final String message) {
    return new SimpleException(message);
  }

  public static SimpleException of(final String message, final Throwable cause) {
    return new SimpleException(message, cause);
  }

  public static SimpleException of(final Throwable cause) {
    return new SimpleException(cause);
  }

  private SimpleException() {
    super();
  }

  private SimpleException(final String message) {
    super(message);
  }

  private SimpleException(final String message, final Throwable cause) {
    super(message, cause);
  }

  private SimpleException(final Throwable cause) {
    super(cause);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    return this;
  }
}
