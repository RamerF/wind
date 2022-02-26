package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;

/**
 * 数据源异常.
 *
 * @since 2022.02.26
 * @author ramer
 */
@Slf4j
public class DataSourceException extends WindException {

  public DataSourceException(final String message) {
    super(message);
  }

  public DataSourceException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public DataSourceException(final Throwable cause) {
    super(cause);
  }
}
