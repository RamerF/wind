package io.github.ramerf.wind.core.jdbc.transaction;

import io.github.ramerf.wind.core.executor.DataAccessException;
import java.sql.Connection;

public interface Transaction {
  Connection getConnection() throws DataAccessException;

  void releaseConnection();

  void commit() throws DataAccessException;

  void rollback() throws DataAccessException;

  void close() throws DataAccessException;

  void setAutoCommit(boolean desiredAutoCommit);

  Integer getTimeout() throws DataAccessException;
}
