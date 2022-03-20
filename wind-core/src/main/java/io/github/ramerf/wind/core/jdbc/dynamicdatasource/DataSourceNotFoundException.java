package io.github.ramerf.wind.core.jdbc.dynamicdatasource;

import io.github.ramerf.wind.core.exception.WindException;

/**
 * @author ramer
 * @since 2022.03.20
 */
public class DataSourceNotFoundException extends WindException {
  public DataSourceNotFoundException(final String message) {
    super(message);
  }

  public DataSourceNotFoundException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public DataSourceNotFoundException(final Throwable cause) {
    super(cause);
  }
}
