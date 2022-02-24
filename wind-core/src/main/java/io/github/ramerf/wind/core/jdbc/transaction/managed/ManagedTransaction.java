package io.github.ramerf.wind.core.jdbc.transaction.managed;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.jdbc.TransactionSynchronizationManager;
import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionException;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import java.sql.SQLException;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ManagedTransaction implements Transaction {
  private final DataSource dataSource;
  private TransactionIsolationLevel level;
  private Connection connection;
  private boolean autoCommit = true;

  public ManagedTransaction(DataSource dataSource) {
    this.dataSource = dataSource;
  }

  public ManagedTransaction(DataSource dataSource, boolean autoCommit) {
    this.dataSource = dataSource;
    this.autoCommit = autoCommit;
  }

  public ManagedTransaction(DataSource ds, TransactionIsolationLevel level, boolean autoCommit) {
    this.dataSource = ds;
    this.level = level;
    this.autoCommit = autoCommit;
  }

  @Override
  public Connection getConnection() throws DataAccessException {
    if (this.connection == null) {
      this.openConnection();
    }
    return this.connection;
  }

  @Override
  public void releaseConnection() {
    TransactionSynchronizationManager.releaseConnection(this.connection, this.dataSource);
  }

  @Override
  public void commit() throws DataAccessException {}

  @Override
  public void rollback() throws DataAccessException {}

  @Override
  public void close() throws DataAccessException {
    if (this.connection != null) {
      if (log.isDebugEnabled()) {
        log.debug("Closing JDBC Connection [" + this.connection + "]");
      }
      DataSourceUtils.close(this.connection);
    }
  }

  @Override
  public void setAutoCommit(final boolean desiredAutoCommit) {
    try {
      if (this.connection.getAutoCommit() != desiredAutoCommit) {
        if (log.isDebugEnabled()) {
          log.debug(
              "Setting autocommit to "
                  + desiredAutoCommit
                  + " on JDBC Connection ["
                  + this.connection
                  + "]");
        }
        this.connection.setAutoCommit(desiredAutoCommit);
      }
    } catch (SQLException e) {
      throw new TransactionException(
          "Error configuring AutoCommit.  Your driver may not support getAutoCommit() or setAutoCommit(). Requested setting: "
              + desiredAutoCommit
              + ".  Cause: "
              + e,
          e);
    }
  }

  protected void openConnection() throws DataAccessException {
    if (log.isDebugEnabled()) {
      log.debug("Opening JDBC Connection");
    }

    this.connection = DataSourceUtils.getConnection(this.dataSource);
    if (this.level != null) {
      try {
        //noinspection MagicConstant
        this.connection.setTransactionIsolation(this.level.getLevel());
      } catch (SQLException e) {
        log.warn("The Connection not support to set TransactionIsolation", e);
      }
    }
  }

  @Override
  public Integer getTimeout() throws DataAccessException {
    return null;
  }
}
