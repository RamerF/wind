package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Pageable;
import io.github.ramerf.wind.core.handler.ResultHandler;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author ramer
 * @since 2022.03.12
 */
public interface Dao {
  <R> R fetchOneBySql(String sql, Class<R> respClazz, Object... args);

  <T, R> List<R> fetchListBySql(String sql, Class<R> respClazz, Object... args);

  <T> long countBySql(String sql, Object... args);

  <T> T fetchOne(@Nonnull Condition<T, ?> condition);

  <T> T fetchOne(@Nonnull Condition<T, ?> condition, Fields<T> fields);

  <T, R> R fetchOne(@Nonnull Condition<T, ?> condition, Fields<T> fields, Class<R> respClazz);

  <T, R> R fetchOne(
      @Nonnull Condition<T, ?> condition,
      Fields<T> fields,
      Class<R> respClazz,
      ResultHandler<R> resultHandler);

  <T> List<T> fetchAll(@Nonnull Condition<T, ?> condition);

  <T> List<T> fetchAll(@Nonnull Condition<T, ?> condition, Fields<T> fields);

  <T, R> List<R> fetchAll(@Nonnull Condition<T, ?> condition, Fields<T> fields, Class<R> respClazz);

  <T, R> List<R> fetchAll(
      @Nonnull Condition<T, ?> condition, Pageable pageable, Fields<T> fields, Class<R> respClazz);

  <T, R> Page<R> fetchPage(
      Condition<T, ?> condition, Pageable pageable, Fields<T> fields, Class<R> respClazz);

  long fetchCount(@Nonnull Condition<?, ?> condition);

  int create(@Nonnull Object t) throws DataAccessException;

  <T> int create(@Nonnull T t, Fields<T> fields) throws DataAccessException;

  Optional<Integer> createBatch(List<?> ts);

  <T> Optional<Integer> createBatch(List<T> ts, Fields<T> fields);

  int update(@Nonnull Object t) throws DataAccessException;

  <T> int update(@Nonnull T t, Fields<T> fields) throws DataAccessException;

  <T> int update(@Nonnull T t, @Nullable Fields<T> fields, @Nonnull Condition<T, ?> condition)
      throws DataAccessException;

  Optional<Integer> updateBatch(@Nonnull List<?> ts);

  <T> Optional<Integer> updateBatch(@Nonnull List<T> ts, Fields<T> fields);

  int delete(@Nonnull Condition<?, ?> condition) throws DataAccessException;

  Configuration getConfiguration();
}
