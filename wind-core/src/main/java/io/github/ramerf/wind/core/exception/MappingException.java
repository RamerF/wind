package io.github.ramerf.wind.core.exception;

import javax.annotation.Nullable;

public class MappingException extends WindException {

  public MappingException(@Nullable String message) {
    super(message);
  }

  public MappingException(@Nullable String message, @Nullable Throwable cause) {
    super(message, cause);
  }
}
