package io.github.ramerf.wind.core.handler;

import java.sql.ResultSet;
import java.sql.SQLException;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Abstract result handler.
 *
 * @since 2020 /4/6
 * @author ramer
 * @param <T> 数据库返回对象
 * @param <E> 实际返回对象
 */
@Slf4j
public abstract class AbstractResultHandler<E> implements ResultHandler<E> {

  @Override
  public abstract E handle(ResultSet rs) throws SQLException;
}
