package io.github.ramerf.wind.core.jdbc.transaction.jdbc;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import java.sql.Connection;
import javax.sql.DataSource;

public class JdbcTransactionFactory implements TransactionFactory {
  public JdbcTransactionFactory() {}

  @Override
  public Transaction newTransaction(Connection connection) {
    return new JdbcTransaction(connection);
  }

  @Override
  public Transaction newTransaction(
      DataSource dataSource,
      TransactionIsolationLevel transactionIsolationLevel,
      boolean autoCommit) {
    return new JdbcTransaction(dataSource, transactionIsolationLevel, autoCommit);
  }
}
