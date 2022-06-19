package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.util.DataSourceUtils;
import java.sql.Connection;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
public class DefaultDaoFactory implements DaoFactory {
  private final Configuration configuration;

  public static DefaultDaoFactory of(final Configuration configuration) {
    return new DefaultDaoFactory(configuration);
  }

  private DefaultDaoFactory(final Configuration configuration) {
    this.configuration = configuration;
  }

  @Override
  public Dao getDao() {
    return getDao(null, false);
  }

  @Override
  public Dao getDao(boolean autoCommit) {
    return getDao(null, autoCommit);
  }

  @Override
  public Dao getDao(TransactionIsolationLevel level) {
    return getDao(level, false);
  }

  @Override
  public Dao getDao(TransactionIsolationLevel level, boolean autoCommit) {
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    final TransactionFactory transactionFactory = jdbcEnvironment.getTransactionFactory();
    final Transaction transaction =
        transactionFactory.newTransaction(jdbcEnvironment.getDataSource(), level, autoCommit);
    Executor executor = new SimpleJdbcExecutor(configuration, transaction);
    final DaoImpl dao = new DaoImpl(configuration, executor, autoCommit);
    return (DaoImpl)
        configuration
            .getDaoInterceptorChain()
            .pluginAll(dao, new Object[] {configuration, executor, autoCommit});
  }

  @Override
  public Dao getDao(final Connection connection) {
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    final TransactionFactory transactionFactory = jdbcEnvironment.getTransactionFactory();
    final Transaction transaction = transactionFactory.newTransaction(connection);
    Executor executor = new SimpleJdbcExecutor(configuration, transaction);
    // 默认true,因为可能数据源或数据库不支持
    final boolean autoCommit = DataSourceUtils.getAutoCommit(connection, true);
    final DaoImpl dao = new DaoImpl(configuration, executor, autoCommit);
    return (DaoImpl)
        configuration
            .getDaoInterceptorChain()
            .pluginAll(dao, new Object[] {configuration, executor, autoCommit});
  }

  @Override
  public Configuration getConfiguration() {
    return configuration;
  }
}
