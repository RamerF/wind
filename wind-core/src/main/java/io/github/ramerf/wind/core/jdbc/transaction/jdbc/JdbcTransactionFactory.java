package io.github.ramerf.wind.core.jdbc.transaction.jdbc;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import javax.sql.DataSource;

public class JdbcTransactionFactory implements TransactionFactory {
  public JdbcTransactionFactory() {}

  @Override
  public Transaction newTransaction(DataSource dataSource) {
    return new JdbcTransaction(dataSource);
  }

  @Override
  public Transaction newTransaction(DataSource dataSource, boolean autoCommit) {
    return new JdbcTransaction(dataSource, autoCommit);
  }

  @Override
  public Transaction newTransaction(
      DataSource dataSource,
      TransactionIsolationLevel transactionIsolationLevel,
      boolean autoCommit) {
    return new JdbcTransaction(dataSource, transactionIsolationLevel, autoCommit);
  }
}
