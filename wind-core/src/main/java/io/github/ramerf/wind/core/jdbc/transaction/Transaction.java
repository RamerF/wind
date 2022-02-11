package io.github.ramerf.wind.core.jdbc.transaction;

import java.sql.Connection;
import java.sql.SQLException;

public interface Transaction {
  Connection getConnection() throws SQLException;

  void commit() throws SQLException;

  void rollback() throws SQLException;

  void close() throws SQLException;

  Integer getTimeout() throws SQLException;
}
