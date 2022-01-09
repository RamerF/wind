package io.github.ramerf.wind.core.executor;

import javax.annotation.Nullable;

public class DataAccessException extends RuntimeException {
  public DataAccessException(String message) {
    super(message);
  }

  public DataAccessException(@Nullable String message, @Nullable Throwable cause) {
    super(message, cause);
  }
}
