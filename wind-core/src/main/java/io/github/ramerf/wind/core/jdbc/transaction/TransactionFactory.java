package io.github.ramerf.wind.core.jdbc.transaction;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import java.sql.Connection;
import java.util.Properties;
import javax.sql.DataSource;

public interface TransactionFactory {
  default void setProperties(Properties props) {}

  Transaction newTransaction(Connection var1);

  Transaction newTransaction(DataSource var1, TransactionIsolationLevel var2, boolean var3);
}
