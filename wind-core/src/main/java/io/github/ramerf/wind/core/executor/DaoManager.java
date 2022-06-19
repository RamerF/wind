package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.*;
import java.sql.Connection;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * 管理线程安全的Dao.
 *
 * @see DaoImpl
 */
public class DaoManager implements DaoFactory, Dao {

  private final DaoFactory daoFactory;
  private final Dao daoProxy;
  private final ThreadLocal<Dao> localDao = new ThreadLocal<>();

  private DaoManager(final DaoFactory daoFactory) {
    this.daoFactory = daoFactory;
    this.daoProxy =
        (Dao)
            Proxy.newProxyInstance(
                DaoFactory.class.getClassLoader(),
                new Class[] {Dao.class},
                new ThreadDaoInterceptor());
  }

  public static DaoManager newInstance(final DaoFactory daoFactory) {
    return new DaoManager(daoFactory);
  }

  public void startManagedDao() {
    this.localDao.set(getDao());
  }

  public void startManagedDao(final boolean autoCommit) {
    this.localDao.set(getDao(autoCommit));
  }

  public void startManagedDao(final Connection connection) {
    this.localDao.set(getDao(connection));
  }

  public void startManagedDao(final TransactionIsolationLevel level) {
    this.localDao.set(getDao(level));
  }

  public void startManagedDao(final TransactionIsolationLevel level, final boolean autoCommit) {
    this.localDao.set(getDao(level, autoCommit));
  }

  public boolean isManagedDaoStarted() {
    return this.localDao.get() != null;
  }

  @Override
  public Dao getDao() {
    return daoFactory.getDao();
  }

  @Override
  public Dao getDao(boolean autoCommit) {
    return daoFactory.getDao(autoCommit);
  }

  @Override
  public Dao getDao(Connection connection) {
    return daoFactory.getDao(connection);
  }

  @Override
  public Dao getDao(TransactionIsolationLevel level) {
    return daoFactory.getDao(level);
  }

  @Override
  public Dao getDao(final TransactionIsolationLevel level, final boolean autoCommit) {
    return daoFactory.getDao(level, autoCommit);
  }

  @Override
  public long fetchCount(@Nonnull final Condition<?, ?> condition) throws DataAccessException {
    return daoProxy.fetchCount(condition);
  }

  @Override
  public long fetchCount(final String sql, final Object... args) throws DataAccessException {
    return daoProxy.fetchCount(sql, args);
  }

  @Override
  public <T> T fetchOne(@Nonnull final Condition<T, ?> condition) throws DataAccessException {
    return daoProxy.fetchOne(condition);
  }

  @Override
  public <T> T fetchOne(@Nonnull final Condition<T, ?> condition, final Fields<T> fields)
      throws DataAccessException {
    return daoProxy.fetchOne(condition, fields);
  }

  @Override
  public <T, R> R fetchOne(@Nonnull final Condition<T, ?> condition, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchOne(condition, respClazz);
  }

  @Override
  public <T, R> R fetchOne(
      @Nonnull final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchOne(condition, fields, respClazz);
  }

  @Override
  public <T, R> R fetchOne(
      @Nonnull final Condition<T, ?> condition,
      final Fields<T> fields,
      final Class<R> respClazz,
      final ResultHandler<R> resultHandler)
      throws DataAccessException {
    return daoProxy.fetchOne(condition, fields, respClazz, resultHandler);
  }

  @Override
  public <R> R fetchOne(final String sql, final Class<R> respClazz, final Object... args)
      throws DataAccessException {
    return daoProxy.fetchOne(sql, respClazz, args);
  }

  @Override
  public <T> List<T> fetchAll(@Nonnull final Condition<T, ?> condition) throws DataAccessException {
    return daoProxy.fetchAll(condition);
  }

  @Override
  public <T> List<T> fetchAll(@Nonnull final Condition<T, ?> condition, final Fields<T> fields)
      throws DataAccessException {
    return daoProxy.fetchAll(condition, fields);
  }

  @Override
  public <T, R> List<R> fetchAll(@Nonnull final Condition<T, ?> condition, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchAll(condition, respClazz);
  }

  @Override
  public <T, R> List<R> fetchAll(
      @Nonnull final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchAll(condition, fields, respClazz);
  }

  @Override
  public <T, R> List<R> fetchAll(final String sql, final Class<R> respClazz, final Object... args)
      throws DataAccessException {
    return daoProxy.fetchAll(sql, respClazz, args);
  }

  @Override
  public <T> Page<T> fetchPage(final Condition<T, ?> condition) throws DataAccessException {
    return daoProxy.fetchPage(condition);
  }

  @Override
  public <T> Page<T> fetchPage(final Condition<T, ?> condition, final Fields<T> fields)
      throws DataAccessException {
    return daoProxy.fetchPage(condition, fields);
  }

  @Override
  public <T, R> Page<R> fetchPage(final Condition<T, ?> condition, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchPage(condition, respClazz);
  }

  @Override
  public <T, R> Page<R> fetchPage(
      final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz)
      throws DataAccessException {
    return daoProxy.fetchPage(condition, fields, respClazz);
  }

  @Override
  public int create(@Nonnull final Object t) throws DataAccessException {
    return daoProxy.create(t);
  }

  @Override
  public <T> int create(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    return daoProxy.create(t, fields);
  }

  @Override
  public Optional<Integer> createBatch(final List<?> ts) throws DataAccessException {
    return daoProxy.createBatch(ts);
  }

  @Override
  public <T> Optional<Integer> createBatch(final List<T> ts, final Fields<T> fields)
      throws DataAccessException {
    return daoProxy.createBatch(ts, fields);
  }

  @Override
  public int update(@Nonnull final Object t) throws DataAccessException {
    return daoProxy.update(t);
  }

  @Override
  public <T> int update(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    return daoProxy.update(t, fields);
  }

  @Override
  public <T> int update(
      @Nonnull final T t,
      @Nullable final Fields<T> fields,
      @Nonnull final Condition<T, ?> condition)
      throws DataAccessException {
    return daoProxy.update(t, fields, condition);
  }

  @Override
  public Optional<Integer> updateBatch(@Nonnull final List<?> ts) throws DataAccessException {
    return daoProxy.updateBatch(ts);
  }

  @Override
  public <T> Optional<Integer> updateBatch(@Nonnull final List<T> ts, final Fields<T> fields)
      throws DataAccessException {
    return daoProxy.updateBatch(ts, fields);
  }

  @Override
  public int update(final String sql, @Nonnull final PreparedStatementSetter pss)
      throws DataAccessException {
    return daoProxy.update(sql, pss);
  }

  @Override
  public int delete(@Nonnull final Condition<?, ?> condition) throws DataAccessException {
    return daoProxy.delete(condition);
  }

  @Override
  public Configuration getConfiguration() {
    return daoFactory.getConfiguration();
  }

  @Override
  public Connection getConnection() {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error: Cannot get connection. No managed dao is started.");
    }
    return dao.getConnection();
  }

  @Override
  public void commit() {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error:  Cannot commit.  No managed session is started.");
    }
    dao.commit();
  }

  @Override
  public void commit(boolean force) {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error:  Cannot commit.  No managed session is started.");
    }
    dao.commit(force);
  }

  @Override
  public void rollback() {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error:  Cannot rollback.  No managed session is started.");
    }
    dao.rollback();
  }

  @Override
  public void rollback(boolean force) {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error:  Cannot rollback.  No managed session is started.");
    }
    dao.rollback(force);
  }

  @Override
  public void close() {
    final Dao dao = localDao.get();
    if (dao == null) {
      throw new DataAccessException("Error: Cannot close. No managed session is started.");
    }
    try {
      dao.close();
    } finally {
      localDao.set(null);
    }
  }

  /** 管理线程安全的dao对象 */
  private class ThreadDaoInterceptor implements InvocationHandler {
    public ThreadDaoInterceptor() {
      // Prevent Synthetic Access
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      final Dao dao = DaoManager.this.localDao.get();
      if (dao != null) {
        try {
          return method.invoke(dao, args);
        } catch (Throwable t) {
          throw ExceptionUtil.unwrapThrowable(t);
        }
      } else {
        try (Dao autoDao = getDao()) {
          try {
            final Object result = method.invoke(autoDao, args);
            autoDao.commit();
            return result;
          } catch (Throwable t) {
            autoDao.rollback();
            throw ExceptionUtil.unwrapThrowable(t);
          }
        }
      }
    }
  }
}
