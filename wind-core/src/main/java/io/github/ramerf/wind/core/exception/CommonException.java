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

  public CommonException() {
    super();
  }

  public CommonException(final String message) {
    super(message);
  }

  public CommonException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public CommonException(final Throwable cause) {
    super(cause);
  }
}
