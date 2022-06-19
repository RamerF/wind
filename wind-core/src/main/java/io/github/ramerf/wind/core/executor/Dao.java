package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.handler.ResultHandler;
import java.io.Closeable;
import java.sql.Connection;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author ramer
 * @since 2022.03.12
 * @see DaoImpl
 * @see DaoManager
 */
public interface Dao extends Closeable {

  long fetchCount(@Nonnull Condition<?, ?> condition) throws DataAccessException;

  long fetchCount(String sql, Object... args) throws DataAccessException;

  <T> T fetchOne(@Nonnull Condition<T, ?> condition) throws DataAccessException;

  <T> T fetchOne(@Nonnull Condition<T, ?> condition, Fields<T> fields) throws DataAccessException;

  <T, R> R fetchOne(@Nonnull Condition<T, ?> condition, Class<R> respClazz)
      throws DataAccessException;

  <T, R> R fetchOne(@Nonnull Condition<T, ?> condition, Fields<T> fields, Class<R> respClazz)
      throws DataAccessException;

  <T, R> R fetchOne(
      @Nonnull Condition<T, ?> condition,
      Fields<T> fields,
      Class<R> respClazz,
      ResultHandler<R> resultHandler)
      throws DataAccessException;

  <R> R fetchOne(String sql, Class<R> respClazz, Object... args) throws DataAccessException;

  <T> List<T> fetchAll(@Nonnull Condition<T, ?> condition) throws DataAccessException;

  <T> List<T> fetchAll(@Nonnull Condition<T, ?> condition, Fields<T> fields)
      throws DataAccessException;

  <T, R> List<R> fetchAll(@Nonnull Condition<T, ?> condition, Class<R> respClazz)
      throws DataAccessException;

  <T, R> List<R> fetchAll(@Nonnull Condition<T, ?> condition, Fields<T> fields, Class<R> respClazz)
      throws DataAccessException;

  <T, R> List<R> fetchAll(String sql, Class<R> respClazz, Object... args)
      throws DataAccessException;

  <T> Page<T> fetchPage(Condition<T, ?> condition) throws DataAccessException;

  <T> Page<T> fetchPage(Condition<T, ?> condition, Fields<T> fields) throws DataAccessException;

  <T, R> Page<R> fetchPage(Condition<T, ?> condition, Class<R> respClazz)
      throws DataAccessException;

  <T, R> Page<R> fetchPage(Condition<T, ?> condition, Fields<T> fields, Class<R> respClazz)
      throws DataAccessException;

  int create(@Nonnull Object t) throws DataAccessException;

  <T> int create(@Nonnull T t, Fields<T> fields) throws DataAccessException;

  Optional<Integer> createBatch(List<?> ts) throws DataAccessException;

  <T> Optional<Integer> createBatch(List<T> ts, Fields<T> fields) throws DataAccessException;

  int update(@Nonnull Object t) throws DataAccessException;

  <T> int update(@Nonnull T t, Fields<T> fields) throws DataAccessException;

  <T> int update(@Nonnull T t, @Nullable Fields<T> fields, @Nonnull Condition<T, ?> condition)
      throws DataAccessException;

  Optional<Integer> updateBatch(@Nonnull List<?> ts) throws DataAccessException;

  <T> Optional<Integer> updateBatch(@Nonnull List<T> ts, Fields<T> fields)
      throws DataAccessException;

  int update(String sql, @Nonnull PreparedStatementSetter pss) throws DataAccessException;

  int delete(@Nonnull Condition<?, ?> condition) throws DataAccessException;

  Configuration getConfiguration();

  void commit() throws DataAccessException;

  void commit(boolean force) throws DataAccessException;

  void rollback();

  void rollback(boolean force);

  @Override
  void close();

  Connection getConnection();
}
