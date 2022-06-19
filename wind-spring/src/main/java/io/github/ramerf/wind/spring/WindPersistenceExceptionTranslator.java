package io.github.ramerf.wind.spring;

import java.sql.SQLException;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.jdbc.support.SQLErrorCodeSQLExceptionTranslator;
import org.springframework.jdbc.support.SQLExceptionTranslator;
import org.springframework.transaction.TransactionException;

/** 默认数据访问异常翻译. */
public class WindPersistenceExceptionTranslator implements PersistenceExceptionTranslator {

  private final Supplier<SQLExceptionTranslator> exceptionTranslatorSupplier;
  private SQLExceptionTranslator exceptionTranslator;

  /**
   * Creates a new {@code PersistenceExceptionTranslator} instance with {@code
   * SQLErrorCodeSQLExceptionTranslator}.
   *
   * @param dataSource DataSource to use to find metadata and establish which error codes are
   *     usable.
   * @param exceptionTranslatorLazyInit if true, the translator instantiates internal stuff only the
   *     first time will have the need to translate exceptions.
   */
  public WindPersistenceExceptionTranslator(
      DataSource dataSource, boolean exceptionTranslatorLazyInit) {
    this(() -> new SQLErrorCodeSQLExceptionTranslator(dataSource), exceptionTranslatorLazyInit);
  }

  /**
   * Creates a new {@code PersistenceExceptionTranslator} instance with specified {@code
   * SQLExceptionTranslator}.
   *
   * @param exceptionTranslatorSupplier Supplier for creating a {@code SQLExceptionTranslator}
   *     instance
   * @param exceptionTranslatorLazyInit if true, the translator instantiates internal stuff only the
   *     first time will have the need to translate exceptions.
   * @since 2.0.3
   */
  public WindPersistenceExceptionTranslator(
      Supplier<SQLExceptionTranslator> exceptionTranslatorSupplier,
      boolean exceptionTranslatorLazyInit) {
    this.exceptionTranslatorSupplier = exceptionTranslatorSupplier;
    if (!exceptionTranslatorLazyInit) {
      this.initExceptionTranslator();
    }
  }

  /** {@inheritDoc} */
  @Override
  public DataAccessException translateExceptionIfPossible(@Nonnull RuntimeException e) {
    if (e instanceof io.github.ramerf.wind.core.executor.DataAccessException) {
      if (e.getCause() instanceof io.github.ramerf.wind.core.executor.DataAccessException) {
        e = (io.github.ramerf.wind.core.executor.DataAccessException) e.getCause();
      }
      if (e.getCause() instanceof SQLException) {
        this.initExceptionTranslator();
        String task = e.getMessage() + "\n";
        SQLException se = (SQLException) e.getCause();
        DataAccessException dae = this.exceptionTranslator.translate(task, null, se);
        return dae != null ? dae : new UncategorizedSQLException(task, null, se);
      } else if (e.getCause() instanceof TransactionException) {
        throw (TransactionException) e.getCause();
      }
      return new WindUncategorizedDataAccessException(e);
    }
    return null;
  }

  /** Initializes the internal translator reference. */
  private synchronized void initExceptionTranslator() {
    if (this.exceptionTranslator == null) {
      this.exceptionTranslator = exceptionTranslatorSupplier.get();
    }
  }
}
