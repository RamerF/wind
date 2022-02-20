package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import java.sql.SQLException;
import java.sql.Statement;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.executor.statement.StatementUtil;

/**
 * The type Base executor.
 *
 * @since 2022.02.20
 * @author ramer
 */
@Slf4j
public abstract class BaseExecutor implements Executor {
  protected Transaction transaction;
  protected Executor wrapper;
  protected Configuration configuration;
  private boolean closed;

  protected BaseExecutor(Configuration configuration, Transaction transaction) {
    this.transaction = transaction;
    this.closed = false;
    this.configuration = configuration;
    this.wrapper = this;
  }

  @Override
  public Transaction getTransaction() {
    if (closed) {
      throw new ExecutorException("Executor was closed.");
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
      throw new ExecutorException("Cannot commit, transaction is already closed");
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
   * @see StatementUtil#applyTransactionTimeout(Statement, Integer, Integer)
   */
  protected void applyTransactionTimeout(Statement statement) throws SQLException {
    StatementUtil.applyTransactionTimeout(
        statement, statement.getQueryTimeout(), transaction.getTimeout());
  }

  @Override
  public void setExecutorWrapper(Executor wrapper) {
    this.wrapper = wrapper;
  }
}
