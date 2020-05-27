package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.function.SqlAggregateFunction;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import javax.annotation.Nonnull;
import lombok.Builder;
import lombok.Getter;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
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
  <R> R fetchOne(@Nonnull SqlParam sqlParam) throws DataAccessException;

  <R> List<R> fetchAll(@Nonnull SqlParam sqlParam, Class<R> clazz) throws DataAccessException;

  <R> List<R> fetchAll(@Nonnull SqlParam sqlParam) throws DataAccessException;

  <R> Page<R> fetchPage(@Nonnull SqlParam sqlParam, long total, PageRequest pageable)
      throws DataAccessException;

  long fetchCount(@Nonnull SqlParam sqlParam);

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
  @Getter
  @Builder
  class SqlParam {
    /** 执行sql. */
    protected String sql;
    /** 返回对象. */
    protected Class<?> clazz;
    /** 查询列,用于映射查询列和bean字段名,填充返回对象. */
    protected List<QueryColumn<?>> queryColumns;
    /** 参数填充起始位置. */
    protected AtomicInteger startIndex;
    /** sql条件,可获取占位符对应的值,用于redis缓存唯一key生成.{@link ICondition#getValues(AtomicInteger)} */
    protected List<ICondition<?>> conditions;
    /** 执行聚合函数,可为空. */
    protected SqlAggregateFunction aggregateFunction;
  }
}
