package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.handler.ResultHandler;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;

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
  <T, R> R fetchOne(
      @Nonnull SqlParam<T> sqlParam, ResultHandler<Map<String, Object>, R> resultHandler)
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
   * Fetch all list.
   *
   * @param <R> the type parameter
   * @param sqlParam the sql param
   * @return the list
   * @throws DataAccessException the data access exception
   */
  <T, R> List<R> fetchAll(@Nonnull SqlParam<T> sqlParam) throws DataAccessException;

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
  <T, R> Page<R> fetchPage(@Nonnull SqlParam<T> sqlParam, long total, PageRequest pageable)
      throws DataAccessException;

  /**
   * Fetch count long.
   *
   * @param sqlParam the sql param
   * @return the long
   */
  <T> long fetchCount(@Nonnull SqlParam<T> sqlParam);

  /**
   * Query for object t.
   *
   * @param <T> the type parameter
   * @param sqlParam the sql param
   * @param args the args
   * @param requiredType the required type
   * @return the t
   * @throws DataAccessException the data access exception
   */
  <T> T queryForObject(@Nonnull final SqlParam<?> sqlParam, Object[] args, Class<T> requiredType)
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

  /**
   * Query t.
   *
   * @param <T> the type parameter
   * @param sqlParam the sql param
   * @param pss the pss
   * @param rse the rse
   * @return the t
   * @throws DataAccessException the data access exception
   */
  <T> T query(
      @Nonnull final SqlParam<?> sqlParam,
      @Nullable PreparedStatementSetter pss,
      ResultSetExtractor<T> rse)
      throws DataAccessException;

  /**
   * Query list.
   *
   * @param <T> the type parameter
   * @param sqlParam the sql param
   * @param pss the pss
   * @param rowMapper the row mapper
   * @return the list
   * @throws DataAccessException the data access exception
   */
  <T> List<T> query(
      @Nonnull final SqlParam<?> sqlParam,
      @Nullable PreparedStatementSetter pss,
      RowMapper<T> rowMapper)
      throws DataAccessException;

  /**
   * Update int.
   *
   * @param clazz the clazz
   * @param psc the psc
   * @param generatedKeyHolder the generated key holder
   * @return the int
   * @throws DataAccessException the data access exception
   */
  int update(
      @Nonnull final Class<?> clazz,
      final PreparedStatementCreator psc,
      final KeyHolder generatedKeyHolder)
      throws DataAccessException;

  /**
   * Update int.
   *
   * @param clazz the clazz
   * @param sql the sql
   * @param pss the pss
   * @return the int
   * @throws DataAccessException the data access exception
   */
  int update(@Nonnull final Class<?> clazz, String sql, @Nullable PreparedStatementSetter pss)
      throws DataAccessException;

  /**
   * Batch update int [ ].
   *
   * @param clazz the clazz
   * @param sql the sql
   * @param pss the pss
   * @return the int [ ]
   * @throws DataAccessException the data access exception
   */
  int[] batchUpdate(
      @Nonnull final Class<?> clazz, String sql, final BatchPreparedStatementSetter pss)
      throws DataAccessException;

  JdbcTemplate getJdbcTemplate();

  /** 主要用于缓存的key生成. */
  @Getter
  // @Builder
  @Setter
  @Accessors(chain = true)
  class SqlParam<T> {
    /** 执行sql. */
    protected String sql;
    /** 返回对象. */
    protected Class<?> clazz;
    /** 查询的实体对象,用于缓存操作. */
    protected Class<T> entityClazz;
    /** 查询列,用于映射查询列和bean字段名,填充返回对象. */
    protected List<QueryColumn<T>> queryColumns;
    /** 参数填充起始位置. */
    protected AtomicInteger startIndex;
    /** sql条件,可获取占位符对应的值,用于redis缓存唯一key生成.{@link Condition#getValues(AtomicInteger)} */
    protected List<Condition<?>> conditions;
    /** 执行聚合函数,可为空. */
    protected AggregateSqlFunction aggregateFunction;

    public Class<?> getEntityClazz() {
      return entityClazz == null ? clazz : entityClazz;
    }
  }
}
