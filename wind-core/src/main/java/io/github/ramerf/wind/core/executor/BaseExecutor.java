package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.executor.logging.Log;
import io.github.ramerf.wind.core.executor.logging.SimpleLog;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * The type Base executor.
 *
 * @since 2022.02.20
 * @author ramer
 */
public abstract class BaseExecutor implements Executor {
  protected Transaction transaction;
  protected Executor wrapper;
  protected Configuration configuration;
  private boolean closed;
  protected Log log;

  protected BaseExecutor(Configuration configuration, Transaction transaction) {
    this(configuration, transaction, new SimpleLog(BaseExecutor.class));
  }

  protected BaseExecutor(Configuration configuration, Transaction transaction, final Log log) {
    this.transaction = transaction;
    this.closed = false;
    this.configuration = configuration;
    this.wrapper = this;
    this.log = log;
  }

  @Override
  public Transaction getTransaction() {
    if (closed) {
      throw new DataAccessException("Executor was closed.");
    }
    return transaction;
  }

  @Override
  public void close(boolean forceRollback) {
    try {
      try {
        rollback(forceRollback);
      } finally {
        if (transaction != null) {
          transaction.close();
        }
      }
    } catch (DataAccessException e) {
      // Ignore. There's nothing that can be done at this point.
      log.warn("Unexpected exception on closing transaction.  Cause: " + e);
    } finally {
      transaction = null;
      closed = true;
    }
  }

  @Override
  public boolean isClosed() {
    return closed;
  }

  @Override
  public void commit(boolean required) throws DataAccessException {
    if (closed) {
      throw new DataAccessException("Cannot commit, transaction is already closed");
    }
    if (required) {
      transaction.commit();
    }
  }

  @Override
  public void rollback(boolean required) throws DataAccessException {
    if (!closed && required) {
      transaction.rollback();
    }
  }

  /**
   * Apply a transaction timeout.
   *
   * @param statement a current statement
   * @throws SQLException if a database access error occurs, this method is called on a closed
   *     <code>Statement</code>
   * @since 3.4.0
   */
  protected void applyTransactionTimeout(Statement statement) throws SQLException {
    final int queryTimeout = statement.getQueryTimeout();
    final Integer transactionTimeout = transaction.getTimeout();
    if (transactionTimeout == null) {
      return;
    }
    if (queryTimeout == 0 || transactionTimeout < queryTimeout) {
      statement.setQueryTimeout(transactionTimeout);
    }
  }

  @Override
  public void setExecutorWrapper(Executor wrapper) {
    this.wrapper = wrapper;
  }

  @Override
  public void setLog(final Log log) {
    this.log = log;
  }
}
