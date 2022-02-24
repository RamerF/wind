package io.github.ramerf.wind.core.jdbc.transaction;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import java.util.Properties;
import javax.sql.DataSource;

public interface TransactionFactory {
  default void setProperties(Properties props) {}

  Transaction newTransaction(DataSource dataSource);

  Transaction newTransaction(DataSource dataSource, boolean autoCommit);

  Transaction newTransaction(
      DataSource dataSource,
      TransactionIsolationLevel transactionIsolationLevel,
      boolean autoCommit);
}
