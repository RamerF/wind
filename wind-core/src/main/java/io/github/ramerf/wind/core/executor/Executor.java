package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import java.util.List;
import java.util.Map;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;

/**
 * The jdbc executor.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/19
 */
public interface Executor {
  <T> T queryForObject(@Nonnull final SqlParam sqlParam, Object[] args, Class<T> requiredType)
      throws DataAccessException;

  List<Map<String, Object>> queryForList(@Nonnull final SqlParam sqlParam, @Nullable Object... args)
      throws DataAccessException;

  <T> T query(
      @Nonnull final SqlParam sqlParam,
      @Nullable PreparedStatementSetter pss,
      ResultSetExtractor<T> rse)
      throws DataAccessException;

  <T> List<T> query(
      @Nonnull final SqlParam sqlParam,
      @Nullable PreparedStatementSetter pss,
      RowMapper<T> rowMapper)
      throws DataAccessException;

  int update(
      @Nonnull final Class<?> clazz,
      final PreparedStatementCreator psc,
      final KeyHolder generatedKeyHolder)
      throws DataAccessException;

  int update(@Nonnull final Class<?> clazz, String sql, @Nullable PreparedStatementSetter pss)
      throws DataAccessException;

  int[] batchUpdate(
      @Nonnull final Class<?> clazz, String sql, final BatchPreparedStatementSetter pss)
      throws DataAccessException;

  /** 主要用于缓存的key生成. */
  class SqlParam {
    protected String sql;
    protected Class<?> clazz;
    protected List<ICondition<?>> conditions;
    protected SqlAggregateFunction aggregateFunction;

    public static SqlParam of(
        final String sql, final Class<?> clazz, final List<ICondition<?>> conditions) {
      SqlParam sqlParam = new SqlParam();
      sqlParam.sql = sql;
      sqlParam.clazz = clazz;
      sqlParam.conditions = conditions;
      return sqlParam;
    }

    public static SqlParam of(
        final String sql,
        final SqlAggregateFunction aggregateFunction,
        final List<ICondition<?>> conditions) {
      SqlParam sqlParam = new SqlParam();
      sqlParam.sql = sql;
      sqlParam.aggregateFunction = aggregateFunction;
      sqlParam.conditions = conditions;
      return sqlParam;
    }
  }
}
