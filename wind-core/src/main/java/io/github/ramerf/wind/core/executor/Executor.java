package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Pageable;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.jdbc.transaction.Transaction;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * The jdbc executor.
 *
 * @author ramer
 * @since 2020 /5/19
 */
public interface Executor {
  /**
   * Fetch one r.
   *
   * @param <R> the type parameter
   * @param sqlParam the sql param
   * @return the r
   * @throws DataAccessException the data access exception
   */
  <T, R> R fetchOne(@Nonnull SqlParam<T> sqlParam) throws DataAccessException;
  /**
   * Fetch one r.
   *
   * @param <R> the type parameter
   * @param sqlParam the sql param
   * @return the r
   * @throws DataAccessException the data access exception
   */
  <T, R> R fetchOne(@Nonnull SqlParam<T> sqlParam, ResultHandler<R> resultHandler)
      throws DataAccessException;

  /**
   * Fetch all list.
   *
   * @param <R> the type parameter
   * @param sqlParam the sql param
   * @param clazz the clazz
   * @return the list
   * @throws DataAccessException the data access exception
   */
  <T, R> List<R> fetchAll(@Nonnull SqlParam<T> sqlParam, Class<R> clazz) throws DataAccessException;

  /**
   * Fetch page page.
   *
   * @param <R> the type parameter
   * @param sqlParam the sql param
   * @param total the total
   * @param pageable the pageable
   * @return the page
   * @throws DataAccessException the data access exception
   */
  <T, R> Page<R> fetchPage(@Nonnull SqlParam<T> sqlParam, Pageable pageable, final long total)
      throws DataAccessException;

  /**
   * Fetch count long.
   *
   * @param sqlParam the sql param
   * @return the long
   */
  <T> long fetchCount(@Nonnull SqlParam<T> sqlParam);

  <T, R> R queryForObject(@Nonnull final SqlParam<?> sqlParam, Object[] args)
      throws DataAccessException;

  /**
   * Query for list list.
   *
   * @param sqlParam the sql param
   * @param args the args
   * @return the list
   * @throws DataAccessException the data access exception
   */
  Map<String, Object> queryForMap(@Nonnull final SqlParam<?> sqlParam, @Nullable Object... args)
      throws DataAccessException;

  /**
   * Query for list list.
   *
   * @param sqlParam the sql param
   * @param args the args
   * @return the list
   * @throws DataAccessException the data access exception
   */
  List<Map<String, Object>> queryForList(
      @Nonnull final SqlParam<?> sqlParam, @Nullable Object... args) throws DataAccessException;

  <T> List<T> query(
      @Nonnull final SqlParam<?> sqlParam,
      @Nullable PreparedStatementSetter pss,
      ResultHandler<T> resultHandler)
      throws DataAccessException;

  /**
   * Update int.
   *
   * @param psc the psc
   * @param generatedKeyHolder the generated key holder
   * @return the int
   * @throws DataAccessException the data access exception
   */
  int update(final PreparedStatementCreator psc, final KeyHolder generatedKeyHolder)
      throws DataAccessException;

  int update(String sql, @Nonnull PreparedStatementSetter pss) throws DataAccessException;

  /** 批量更新. */
  int[] batchUpdate(String sql, final BatchPreparedStatementSetter pss) throws DataAccessException;

  /** 批量创建,填充主键. */
  int[] batchUpdate(
      PreparedStatementCreator psc,
      final BatchPreparedStatementSetter pss,
      final KeyHolder generatedKeyHolder)
      throws DataAccessException;

  Transaction getTransaction();

  void close(boolean forceRollback);

  boolean isClosed();

  void commit(boolean required) throws DataAccessException;

  void rollback(boolean required) throws DataAccessException;

  void setExecutorWrapper(Executor wrapper);

  @Getter
  @Setter
  @Accessors(chain = true)
  class SqlParam<T> {
    /** 执行sql. */
    protected String sql;
    /** 返回对象. */
    protected Class<?> clazz;
    /** 查询的实体对象,用于缓存操作. */
    protected Class<T> entityClazz;
    /** 参数填充起始位置. */
    protected AtomicInteger startIndex = new AtomicInteger(1);
    /** sql条件,可获取占位符对应的值,用于redis缓存唯一key生成.{@link Condition#getValues(AtomicInteger)} */
    protected Condition<?, ?> condition;
    /** 执行聚合函数,可为空. */
    protected AggregateSqlFunction aggregateFunction;

    public Class<?> getEntityClazz() {
      return entityClazz == null ? clazz : entityClazz;
    }
  }
}
