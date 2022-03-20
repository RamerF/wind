package io.github.ramerf.wind.core.jdbc.transaction.jdbc;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.jdbc.ConnectionHolder;
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
public class JdbcTransaction implements Transaction {
  protected DataSource dataSource;
  protected Connection connection;
  protected ConnectionHolder connectionHolder;
  protected TransactionIsolationLevel level;
  protected boolean autoCommit;

  public JdbcTransaction(final Connection connection) {
    this.connection = connection;
  }

  public JdbcTransaction(
      DataSource dataSource, TransactionIsolationLevel desiredLevel, boolean autoCommit) {
    this.dataSource = dataSource;
    this.level = desiredLevel;
    this.autoCommit = autoCommit;
  }

  @Override
  public Connection getConnection() throws DataAccessException {
    if (this.connection == null) {
      this.openConnection();
      return this.connectionHolder.getConnection();
    }
    return this.connectionHolder != null ? this.connectionHolder.requestConnection() : connection;
  }

  @Override
  public void releaseConnection() {
    TransactionSynchronizationManager.releaseConnection(this.connection, this.dataSource);
  }

  @Override
  public void commit() throws DataAccessException {
    if (this.connection != null && !DataSourceUtils.getAutoCommit(this.connection)) {
      if (log.isDebugEnabled()) {
        log.debug("Committing JDBC Connection [" + this.connection + "]");
      }
      DataSourceUtils.commit(this.connection);
    }
  }

  @Override
  public void rollback() throws DataAccessException {
    if (this.connection != null && !DataSourceUtils.getAutoCommit(this.connection)) {
      if (log.isDebugEnabled()) {
        log.debug("Rolling back JDBC Connection [" + this.connection + "]");
      }
      DataSourceUtils.rollback(this.connection);
    }
  }

  @Override
  public void close() throws DataAccessException {
    if (this.connection != null) {
      this.resetAutoCommit();
      if (log.isDebugEnabled()) {
        log.debug("Closing JDBC Connection [" + this.connection + "]");
      }
      DataSourceUtils.close(this.connection);
    }
  }

  @Override
  public void setAutoCommit(boolean desiredAutoCommit) {
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

  protected void resetAutoCommit() {
    try {
      if (!this.connection.getAutoCommit()) {
        if (log.isDebugEnabled()) {
          log.debug("Resetting autocommit to true on JDBC Connection [" + this.connection + "]");
        }
        this.connection.setAutoCommit(true);
      }
    } catch (SQLException e) {
      if (log.isDebugEnabled()) {
        log.debug("Error resetting autocommit to true before closing the connection.  Cause: " + e);
      }
    }
  }

  protected void openConnection() throws DataAccessException {
    if (log.isDebugEnabled()) {
      log.debug("Opening JDBC Connection");
    }
    final ConnectionHolder connectionHolder =
        TransactionSynchronizationManager.getConnectionHolder(this.dataSource);
    this.connection = connectionHolder.getConnection();
    this.connectionHolder = connectionHolder;
    if (this.level != null) {
      try {
        //noinspection MagicConstant
        this.connection.setTransactionIsolation(this.level.getLevel());
      } catch (SQLException e) {
        log.warn("The Connection not support to set TransactionIsolation", e);
      }
    }
    this.setAutoCommit(this.autoCommit);
  }

  @Override
  public Integer getTimeout() throws DataAccessException {
    return null;
  }
}
