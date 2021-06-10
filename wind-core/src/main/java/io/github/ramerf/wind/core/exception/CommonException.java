package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 全局通用异常.
 *
 * @since 2020.12.22
 * @author ramer
 */
@Slf4j
public class CommonException extends RuntimeException {

  public static CommonException of() {
    return new CommonException();
  }

  public static CommonException of(final String message) {
    return new CommonException(message);
  }

  public static CommonException of(final String message, final Throwable cause) {
    return new CommonException(message, cause);
  }

  public static CommonException of(final Throwable cause) {
    return new CommonException(cause);
  }

  protected CommonException() {
    super();
  }

  protected CommonException(final String message) {
    super(message);
  }

  protected CommonException(final String message, final Throwable cause) {
    super(message, cause);
  }

  protected CommonException(final Throwable cause) {
    super(cause);
  }
}
