package io.github.ramerf.wind.core.handler;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 返回结果转换.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author ramer
 * @since 2019 /12/27
 * @see AbstractResultHandler
 */
public interface ResultHandler<E> {
  E handle(ResultSet rs) throws SQLException;
}
