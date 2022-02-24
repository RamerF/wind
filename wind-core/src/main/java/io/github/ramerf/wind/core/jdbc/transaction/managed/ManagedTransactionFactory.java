package io.github.ramerf.wind.core.jdbc.transaction.managed;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import java.util.Properties;
import javax.sql.DataSource;

public class ManagedTransactionFactory implements TransactionFactory {
  private boolean closeConnection = true;

  public ManagedTransactionFactory() {}

  @Override
  public void setProperties(Properties props) {
    if (props != null) {
      String closeConnectionProperty = props.getProperty("closeConnection");
      if (closeConnectionProperty != null) {
        this.closeConnection = Boolean.parseBoolean(closeConnectionProperty);
      }
    }
  }

  @Override
  public Transaction newTransaction(DataSource dataSource) {
    return new ManagedTransaction(dataSource);
  }

  @Override
  public Transaction newTransaction(final DataSource dataSource, final boolean autoCommit) {
    return new ManagedTransaction(dataSource, autoCommit);
  }

  @Override
  public Transaction newTransaction(
      DataSource dataSource,
      TransactionIsolationLevel transactionIsolationLevel,
      boolean autoCommit) {
    return new ManagedTransaction(dataSource, transactionIsolationLevel, autoCommit);
  }
}
