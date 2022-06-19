package io.github.ramerf.wind.spring;

import org.springframework.dao.UncategorizedDataAccessException;

public class WindUncategorizedDataAccessException extends UncategorizedDataAccessException {

  public WindUncategorizedDataAccessException(Throwable cause) {
    super(null, cause);
  }
}
