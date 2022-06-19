package io.github.ramerf.wind.spring;

import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.DaoFactory;
import io.github.ramerf.wind.core.util.Asserts;
import io.github.ramerf.wind.spring.transaction.SpringManagedTransactionFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.TransientDataAccessResourceException;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.jdbc.datasource.DataSourceUtils;
import org.springframework.transaction.support.TransactionSynchronizationAdapter;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Slf4j
public final class DaoUtils {
  private static final String NO_DAO_FACTORY_SPECIFIED = "No DaoFactory specified";
  private static final String NO_DAO_SPECIFIED = "No Dao specified";

  /** This class can't be instantiated, exposes static utility methods only. */
  private DaoUtils() {
    // do nothing
  }

  public static Dao getDao(DaoFactory daoFactory) {
    return getDao(daoFactory, null);
  }

  public static Dao getDao(
      DaoFactory daoFactory, PersistenceExceptionTranslator exceptionTranslator) {
    Asserts.notNull(daoFactory, NO_DAO_FACTORY_SPECIFIED);
    DaoHolder holder = (DaoHolder) TransactionSynchronizationManager.getResource(daoFactory);
    Dao dao = daoHolder(holder);
    if (dao != null) {
      return dao;
    }

    log.debug("Creating a new Dao");
    dao = daoFactory.getDao();
    registerDaoHolder(daoFactory, exceptionTranslator, dao);
    return dao;
  }

  private static void registerDaoHolder(
      DaoFactory daoFactory, PersistenceExceptionTranslator exceptionTranslator, Dao dao) {
    DaoHolder holder;
    if (TransactionSynchronizationManager.isSynchronizationActive()) {
      JdbcEnvironment environment = daoFactory.getConfiguration().getJdbcEnvironment();
      if (environment.getTransactionFactory() instanceof SpringManagedTransactionFactory) {
        log.debug("Registering transaction synchronization for Dao [" + dao + "]");

        holder = new DaoHolder(dao, exceptionTranslator);
        TransactionSynchronizationManager.bindResource(daoFactory, holder);
        TransactionSynchronizationManager.registerSynchronization(
            new DaoSynchronizationAdapter(holder, daoFactory));
        holder.setSynchronizedWithTransaction(true);
        holder.requested();
      } else {
        if (TransactionSynchronizationManager.getResource(environment.getDataSource()) == null) {
          log.debug(
              "Dao ["
                  + dao
                  + "] was not registered for synchronization because DataSource is not transactional");
        } else {
          throw new TransientDataAccessResourceException(
              "DaoFactory must be using a SpringManagedTransactionFactory in order to use Spring transaction synchronization");
        }
      }
    } else {
      log.debug(
          "Dao ["
              + dao
              + "] was not registered for synchronization because synchronization is not active");
    }
  }

  private static Dao daoHolder(DaoHolder holder) {
    Dao dao = null;
    if (holder != null && holder.isSynchronizedWithTransaction()) {
      holder.requested();
      log.debug("Fetched Dao [" + holder.getDao() + "] from current transaction");
      dao = holder.getDao();
    }
    return dao;
  }

  /**
   * Checks if {@code Dao} passed as an argument is managed by Spring {@code
   * TransactionSynchronizationManager} If it is not, it closes it, otherwise it just updates the
   * reference counter and lets Spring call the close callback when the managed transaction ends
   *
   * @param dao a target Dao
   * @param daoFactory a factory of Dao
   */
  public static void closeDao(Dao dao, DaoFactory daoFactory) {
    Asserts.notNull(dao, NO_DAO_SPECIFIED);
    Asserts.notNull(daoFactory, NO_DAO_FACTORY_SPECIFIED);

    DaoHolder holder = (DaoHolder) TransactionSynchronizationManager.getResource(daoFactory);
    if ((holder != null) && (holder.getDao() == dao)) {
      log.debug("Releasing transactional Dao [" + dao + "]");
      holder.released();
    } else {
      log.debug("Closing non transactional Dao [" + dao + "]");
      dao.close();
    }
  }

  public static boolean isDaoTransactional(Dao dao, DaoFactory daoFactory) {
    Asserts.notNull(dao, NO_DAO_SPECIFIED);
    Asserts.notNull(daoFactory, NO_DAO_FACTORY_SPECIFIED);
    DaoHolder holder = (DaoHolder) TransactionSynchronizationManager.getResource(daoFactory);
    return (holder != null) && (holder.getDao() == dao);
  }

  /**
   * Callback for cleaning up resources. It cleans TransactionSynchronizationManager and also
   * commits and closes the {@code Dao}. It assumes that {@code Connection} life cycle will be
   * managed by {@code DataSourceTransactionManager} or {@code JtaTransactionManager}
   */
  private static final class DaoSynchronizationAdapter extends TransactionSynchronizationAdapter {
    private final DaoHolder holder;
    private final DaoFactory daoFactory;
    private boolean holderActive = true;

    public DaoSynchronizationAdapter(DaoHolder holder, DaoFactory daoFactory) {
      Asserts.notNull(holder, "Parameter 'holder' must be not null");
      Asserts.notNull(daoFactory, "Parameter 'daoFactory' must be not null");

      this.holder = holder;
      this.daoFactory = daoFactory;
    }

    /** {@inheritDoc} */
    @Override
    public int getOrder() {
      // order right before any Connection synchronization
      return DataSourceUtils.CONNECTION_SYNCHRONIZATION_ORDER - 1;
    }

    /** {@inheritDoc} */
    @Override
    public void suspend() {
      if (this.holderActive) {
        log.debug("Transaction synchronization suspending Dao [" + this.holder.getDao() + "]");
        TransactionSynchronizationManager.unbindResource(this.daoFactory);
      }
    }

    /** {@inheritDoc} */
    @Override
    public void resume() {
      if (this.holderActive) {
        log.debug("Transaction synchronization resuming Dao [" + this.holder.getDao() + "]");
        TransactionSynchronizationManager.bindResource(this.daoFactory, this.holder);
      }
    }

    /** {@inheritDoc} */
    @Override
    public void beforeCommit(boolean readOnly) {
      if (TransactionSynchronizationManager.isActualTransactionActive()) {
        try {
          log.debug("Transaction synchronization committing Dao [" + this.holder.getDao() + "]");
          this.holder.getDao().commit();
        } catch (WindUncategorizedDataAccessException p) {
          if (this.holder.getPersistenceExceptionTranslator() != null) {
            DataAccessException translated =
                this.holder.getPersistenceExceptionTranslator().translateExceptionIfPossible(p);
            if (translated != null) {
              throw translated;
            }
          }
          throw p;
        }
      }
    }

    /** {@inheritDoc} */
    @Override
    public void beforeCompletion() {
      if (!this.holder.isOpen()) {
        log.debug("Transaction synchronization deregistering Dao [" + this.holder.getDao() + "]");
        TransactionSynchronizationManager.unbindResource(daoFactory);
        this.holderActive = false;
        log.debug("Transaction synchronization closing Dao [" + this.holder.getDao() + "]");
        this.holder.getDao().close();
      }
    }

    /** {@inheritDoc} */
    @Override
    public void afterCompletion(int status) {
      if (this.holderActive) {
        log.debug("Transaction synchronization deregistering Dao [" + this.holder.getDao() + "]");
        TransactionSynchronizationManager.unbindResourceIfPossible(daoFactory);
        this.holderActive = false;
        log.debug("Transaction synchronization closing Dao [" + this.holder.getDao() + "]");
        this.holder.getDao().close();
      }
      this.holder.reset();
    }
  }
}
