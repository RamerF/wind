package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.Getter;

/** jdbc环境. */
public final class JdbcEnvironment {
  @Getter private final TransactionFactory transactionFactory;
  @Getter private final DataSource dataSource;

  public JdbcEnvironment(
      @Nonnull final TransactionFactory transactionFactory, @Nonnull final DataSource dataSource) {
    this.transactionFactory = transactionFactory;
    this.dataSource = dataSource;
  }
}
