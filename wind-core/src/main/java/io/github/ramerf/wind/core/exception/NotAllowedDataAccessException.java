package io.github.ramerf.wind.core.exception;

import io.github.ramerf.wind.core.executor.DataAccessException;
import lombok.extern.slf4j.Slf4j;

/**
 * 不允许的数据库操作.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class NotAllowedDataAccessException extends DataAccessException {
  public NotAllowedDataAccessException(String msg) {
    super(msg);
  }
}
