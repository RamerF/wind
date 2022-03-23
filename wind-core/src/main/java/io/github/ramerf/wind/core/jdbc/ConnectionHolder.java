package io.github.ramerf.wind.core.jdbc;

import java.sql.Connection;

/**
 * @author ramer
 * @since 2022.03.23
 */
public interface ConnectionHolder {
  void setConnection(final Connection connection);

  Connection getConnection();

  Connection requestConnection();

  void releaseConnection();
}
