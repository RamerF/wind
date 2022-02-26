package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 类初始化异常.
 *
 * @since 2022.02.11
 * @author ramer
 */
@Slf4j
public class ClassInstantiationException extends WindException {

  public ClassInstantiationException(final String message) {
    super(message);
  }

  public ClassInstantiationException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public ClassInstantiationException(final Throwable cause) {
    super(cause);
  }
}
