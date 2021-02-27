package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 无效的枚举.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class InvalidEnumException extends RuntimeException {
  private final Object value;

  public InvalidEnumException(final String value) {
    this.value = value;
  }

  public InvalidEnumException(final int value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return "Invalid enum [" + value + "]";
  }
}
