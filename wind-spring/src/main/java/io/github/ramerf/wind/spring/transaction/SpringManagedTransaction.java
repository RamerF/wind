package io.github.ramerf.wind.spring.transaction;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionException;
import java.sql.Connection;
import java.sql.SQLException;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.datasource.ConnectionHolder;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import static org.springframework.util.Assert.notNull;

/**
 * {@code SpringManagedTransaction} handles the lifecycle of a JDBC connection. It retrieves a
 * connection from Spring's transaction manager and returns it back to it when it is no longer
 * needed.
 *
 * <p>If Spring's transaction handling is active it will no-op all commit/rollback/close calls
 * assuming that the Spring transaction manager will do the job.
 *
 * <p>If it is not it will behave like {@code JdbcTransaction}.
 *
 * @author Hunter Presnall
 * @author Eduardo Macarron
 */
@Slf4j
public class SpringManagedTransaction implements Transaction {

  private final DataSource dataSource;

  private Connection connection;

  private boolean isConnectionTransactional;

  private boolean autoCommit;

  public SpringManagedTransaction(DataSource dataSource) {
    notNull(dataSource, "No DataSource specified");
    this.dataSource = dataSource;
  }

  /** {@inheritDoc} */
  @Override
  public Connection getConnection() throws DataAccessException {
    if (this.connection == null
        || io.github.ramerf.wind.core.util.DataSourceUtils.isClosed(this.connection)) {
      openConnection();
    }
    return this.connection;
  }

  @Override
  public void releaseConnection() {}

  /**
   * Gets a connection from Spring transaction manager and discovers if this {@code Transaction}
   * should manage connection or let it to Spring.
   *
   * <p>It also reads autocommit setting because when using Spring Transaction MyBatis thinks that
   * autocommit is always false and will always call commit/rollback so we need to no-op that calls.
   */
  private void openConnection() throws DataAccessException {
    if (log.isDebugEnabled()) {
      log.debug(Thread.currentThread().getName() + " Opening JDBC Connection");
    }
    this.connection = DataSourceUtils.getConnection(this.dataSource);
    setAutoCommit(io.github.ramerf.wind.core.util.DataSourceUtils.getAutoCommit(this.connection));
    this.isConnectionTransactional =
        DataSourceUtils.isConnectionTransactional(this.connection, this.dataSource);

    log.debug(
        "JDBC Connection ["
            + this.connection
            + "] will"
            + (this.isConnectionTransactional ? " " : " not ")
            + "be managed by Spring");
  }

  /** {@inheritDoc} */
  @Override
  public void commit() throws DataAccessException {
    if (this.connection != null && !this.isConnectionTransactional && !this.autoCommit) {
      log.debug("Committing JDBC Connection [" + this.connection + "]");
      io.github.ramerf.wind.core.util.DataSourceUtils.commit(this.connection);
    }
  }

  /** {@inheritDoc} */
  @Override
  public void rollback() throws DataAccessException {
    if (this.connection != null && !this.isConnectionTransactional && !this.autoCommit) {
      log.debug("Rolling back JDBC Connection [" + this.connection + "]");
      io.github.ramerf.wind.core.util.DataSourceUtils.rollback(this.connection);
    }
  }

  /** {@inheritDoc} */
  @Override
  public void close() throws DataAccessException {
    DataSourceUtils.releaseConnection(this.connection, this.dataSource);
  }

  @Override
  public void setAutoCommit(final boolean autoCommit) {
    try {
      if (this.connection.getAutoCommit() != autoCommit) {
        if (log.isDebugEnabled()) {
          log.debug(
              Thread.currentThread().getName()
                  + " Setting autocommit to "
                  + autoCommit
                  + " on JDBC Connection ["
                  + this.connection
                  + "]");
        }

        this.connection.setAutoCommit(autoCommit);
      }

    } catch (SQLException var3) {
      throw new TransactionException(
          "Error configuring AutoCommit.  Your driver may not support getAutoCommit() or setAutoCommit(). Requested setting: "
              + autoCommit
              + ".  Cause: "
              + var3,
          var3);
    }
  }

  /** {@inheritDoc} */
  @Override
  public Integer getTimeout() throws DataAccessException {
    ConnectionHolder holder =
        (ConnectionHolder) TransactionSynchronizationManager.getResource(dataSource);
    if (holder != null && holder.hasTimeout()) {
      return holder.getTimeToLiveInSeconds();
    }
    return null;
  }
}
