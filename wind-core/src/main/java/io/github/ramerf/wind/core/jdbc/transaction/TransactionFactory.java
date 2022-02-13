package io.github.ramerf.wind.core.jdbc.transaction;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import java.sql.Connection;
import java.util.Properties;
import javax.sql.DataSource;

public interface TransactionFactory {
  default void setProperties(Properties props) {}

  Transaction newTransaction(Connection connection);

  Transaction newTransaction(
      DataSource dataSource,
      TransactionIsolationLevel transactionIsolationLevel,
      boolean autoCommit);
}
