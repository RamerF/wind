package io.github.ramerf.wind.core.jdbc.transaction.managed;

import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import java.sql.Connection;
import java.util.Properties;
import javax.sql.DataSource;

public class ManagedTransactionFactory implements TransactionFactory {
  private boolean closeConnection = true;

  public ManagedTransactionFactory() {}

  public void setProperties(Properties props) {
    if (props != null) {
      String closeConnectionProperty = props.getProperty("closeConnection");
      if (closeConnectionProperty != null) {
        this.closeConnection = Boolean.parseBoolean(closeConnectionProperty);
      }
    }
  }

  public Transaction newTransaction(Connection conn) {
    return new ManagedTransaction(conn, this.closeConnection);
  }

  public Transaction newTransaction(
      DataSource ds, TransactionIsolationLevel level, boolean autoCommit) {
    return new ManagedTransaction(ds, level, this.closeConnection);
  }
}
