package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.exception.WindException;
import javax.annotation.Nullable;

public class DataAccessException extends WindException {
  public DataAccessException(String message) {
    super(message);
  }

  public DataAccessException(@Nullable String message, @Nullable Throwable cause) {
    super(message, cause);
  }
}
