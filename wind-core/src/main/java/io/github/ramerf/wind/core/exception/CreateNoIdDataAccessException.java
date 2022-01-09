package io.github.ramerf.wind.core.exception;

import io.github.ramerf.wind.core.executor.DataAccessException;
import lombok.extern.slf4j.Slf4j;

/**
 * 创建为返回主键的数据库操作.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class CreateNoIdDataAccessException extends DataAccessException {
  public CreateNoIdDataAccessException(String msg) {
    super(msg);
  }
}
