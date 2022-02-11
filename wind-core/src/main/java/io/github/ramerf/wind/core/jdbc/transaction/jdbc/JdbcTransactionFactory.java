package io.github.ramerf.wind.core.jdbc.transaction.jdbc;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import java.sql.Connection;
import javax.sql.DataSource;

public class JdbcTransactionFactory implements TransactionFactory {
  public JdbcTransactionFactory() {}

  public Transaction newTransaction(Connection conn) {
    return new JdbcTransaction(conn);
  }

  public Transaction newTransaction(
      DataSource ds, TransactionIsolationLevel level, boolean autoCommit) {
    return new JdbcTransaction(ds, level, autoCommit);
  }
}
