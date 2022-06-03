package io.github.ramerf.wind.spring.transaction;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import java.sql.Connection;
import java.util.Properties;
import javax.sql.DataSource;

/**
 * Creates a {@code SpringManagedTransaction}.
 *
 * @author Hunter Presnall
 */
public class SpringManagedTransactionFactory implements TransactionFactory {

  /** {@inheritDoc} */
  @Override
  public Transaction newTransaction(
      DataSource dataSource, TransactionIsolationLevel level, boolean autoCommit) {
    return new SpringManagedTransaction(dataSource);
  }

  /** {@inheritDoc} */
  @Override
  public Transaction newTransaction(Connection conn) {
    throw new UnsupportedOperationException("New Spring transactions require a DataSource");
  }

  /** {@inheritDoc} */
  @Override
  public void setProperties(Properties props) {
    // not needed in this version
  }
}
