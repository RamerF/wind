package io.github.ramerf.wind.spring;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.dao.support.PersistenceExceptionTranslator;

import static java.lang.reflect.Proxy.newProxyInstance;
import static org.springframework.util.Assert.notNull;

/**
 * 线程安全的Dao实现.
 *
 * @author ramer
 * @since 2022.06.18
 * @see Dao
 * @see DaoFactory
 * @see DaoImpl
 */
public class DaoTemplate implements Dao, DisposableBean {
  private final Dao daoProxy;
  private final DaoFactory daoFactory;
  private final PersistenceExceptionTranslator exceptionTranslator;

  public DaoTemplate(DaoFactory daoFactory) {
    this(
        daoFactory,
        new WindPersistenceExceptionTranslator(
            daoFactory.getConfiguration().getJdbcEnvironment().getDataSource(), true));
  }

  public DaoTemplate(DaoFactory daoFactory, PersistenceExceptionTranslator exceptionTranslator) {
    notNull(daoFactory, "Property 'daoFactory' is required");

    this.daoFactory = daoFactory;
    this.exceptionTranslator = exceptionTranslator;
    this.daoProxy =
        (Dao)
            newProxyInstance(
                DaoFactory.class.getClassLoader(), new Class[] {Dao.class}, new DaoInterceptor());
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
    return daoProxy.getConfiguration();
  }

  @Override
  public void commit() throws DataAccessException {
    throw new UnsupportedOperationException(
        "Manual commit is not allowed over a Spring managed Dao");
  }

  @Override
  public void commit(final boolean force) throws DataAccessException {
    throw new UnsupportedOperationException(
        "Manual commit is not allowed over a Spring managed Dao");
  }

  @Override
  public void rollback() {
    throw new UnsupportedOperationException(
        "Manual rollback is not allowed over a Spring managed Dao");
  }

  @Override
  public void rollback(final boolean force) {
    throw new UnsupportedOperationException(
        "Manual rollback is not allowed over a Spring managed Dao");
  }

  @Override
  public void close() {
    throw new UnsupportedOperationException(
        "Manual close is not allowed over a Spring managed Dao");
  }

  @Override
  public Connection getConnection() {
    return daoProxy.getConnection();
  }

  @Override
  public void destroy() {}

  private class DaoInterceptor implements InvocationHandler {

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
      Dao dao = DaoUtils.getDao(DaoTemplate.this.daoFactory, DaoTemplate.this.exceptionTranslator);
      try {
        Object result = method.invoke(dao, args);
        if (!DaoUtils.isDaoTransactional(dao, DaoTemplate.this.daoFactory)) {
          dao.commit(true);
        }
        return result;
      } catch (Throwable t) {
        Throwable unwrapped = ExceptionUtil.unwrapThrowable(t);
        if (DaoTemplate.this.exceptionTranslator != null
            && unwrapped instanceof WindUncategorizedDataAccessException) {
          DaoUtils.closeDao(dao, DaoTemplate.this.daoFactory);
          dao = null;
          Throwable translated =
              DaoTemplate.this.exceptionTranslator.translateExceptionIfPossible(
                  (WindUncategorizedDataAccessException) unwrapped);
          if (translated != null) {
            unwrapped = translated;
          }
        }
        throw unwrapped;
      } finally {
        if (dao != null) {
          DaoUtils.closeDao(dao, DaoTemplate.this.daoFactory);
        }
      }
    }
  }
}
