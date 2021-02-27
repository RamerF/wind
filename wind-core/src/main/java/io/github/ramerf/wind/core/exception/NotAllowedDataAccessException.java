package io.github.ramerf.wind.core.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;

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
