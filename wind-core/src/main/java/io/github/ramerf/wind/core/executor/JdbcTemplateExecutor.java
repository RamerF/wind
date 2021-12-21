package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.exception.TooManyResultException;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.handler.ResultHandlerUtil;
import io.github.ramerf.wind.core.util.CollectionUtils;
import io.github.ramerf.wind.core.util.PageUtils;
import java.util.*;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.*;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;

/**
 * The jdbc template executor.
 *
 * @author ramer
 * @since 2020/5/19
 */
@Slf4j
@SuppressWarnings("DuplicatedCode")
public class JdbcTemplateExecutor implements Executor {
  @Resource @Getter private JdbcTemplate jdbcTemplate;

  @Override
  public <T, R> R fetchOne(@Nonnull final SqlParam<T> sqlParam) throws DataAccessException {
    return fetchOne(sqlParam, null);
  }

  @Override
  public <T, R> R fetchOne(
      @Nonnull final SqlParam<T> sqlParam, ResultHandler<Map<String, Object>, R> resultHandler)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          final List<Map<String, Object>> result =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                  new ColumnMapRowMapper());
          if (CollectionUtils.isEmpty(result)) {
            return null;
          }
          if (result.size() > 1) {
            throw new TooManyResultException(result.size());
          }
          @SuppressWarnings("unchecked")
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          return ResultHandlerUtil.handle(result.get(0), clazz, resultHandler);
        });
  }

  @Override
  public <T, R> List<R> fetchAll(@Nonnull final SqlParam<T> sqlParam, final Class<R> clazz)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          final List<Map<String, Object>> list =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                  new ColumnMapRowMapper());
          if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
          }
          return ResultHandlerUtil.handle(list, clazz);
        });
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T, R> Page<R> fetchPage(
      @Nonnull final SqlParam<T> sqlParam, final long total, final Pageable page)
      throws DataAccessException {
    return aroundRead(
        sqlParam,
        () -> {
          final List<Map<String, Object>> list =
              total < 1
                  ? null
                  : query(
                      sqlParam,
                      ps ->
                          sqlParam
                              .condition
                              .getValues(sqlParam.startIndex)
                              .forEach(o -> o.accept(ps)),
                      new ColumnMapRowMapper());
          final Pageable pageable = page == null ? PageRequest.of(0, Integer.MAX_VALUE) : page;
          // 从0开始
          final int currentPage = pageable.getPageNumber();
          // 每页大小
          final int pageSize = pageable.getPageSize();
          if (CollectionUtils.isEmpty(list)) {
            return PageUtils.toPage(Collections.emptyList(), 0, currentPage, pageSize);
          }
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          return PageUtils.toPage(
              ResultHandlerUtil.handle(list, clazz),
              total,
              currentPage,
              pageSize,
              pageable.getSort());
        });
  }

  @Override
  public <T> long fetchCount(@Nonnull final SqlParam<T> sqlParam) {
    return aroundRead(
        sqlParam,
        () ->
            query(
                sqlParam,
                ps -> sqlParam.condition.getValues(sqlParam.startIndex).forEach(o -> o.accept(ps)),
                rs -> {
                  if (rs.next()) {
                    return rs.getLong(1);
                  }
                  return 0L;
                }));
  }

  @Override
  public <T> T queryForObject(
      @Nonnull final SqlParam<?> sqlParam, final Object[] args, final Class<T> requiredType)
      throws DataAccessException {
    return aroundRead(
        sqlParam, () -> jdbcTemplate.queryForObject(sqlParam.sql, args, requiredType));
  }

  @Override
  public Map<String, Object> queryForMap(@Nonnull final SqlParam<?> sqlParam, final Object... args)
      throws DataAccessException {
    return aroundRead(sqlParam, () -> jdbcTemplate.queryForMap(sqlParam.sql, args));
  }

  @Override
  public List<Map<String, Object>> queryForList(
      @Nonnull final SqlParam<?> sqlParam, final Object... args) throws DataAccessException {
    return aroundRead(sqlParam, () -> jdbcTemplate.queryForList(sqlParam.sql, args));
  }

  @Override
  public <T> T query(
      @Nonnull final SqlParam<?> sqlParam,
      final PreparedStatementSetter pss,
      final ResultSetExtractor<T> rse)
      throws DataAccessException {
    return jdbcTemplate.query(sqlParam.sql, pss, rse);
  }

  @Override
  public <T> List<T> query(
      @Nonnull final SqlParam<?> sqlParam,
      final PreparedStatementSetter pss,
      final RowMapper<T> rowMapper)
      throws DataAccessException {
    return jdbcTemplate.query(sqlParam.sql, pss, rowMapper);
  }

  @Override
  public int update(
      @Nonnull final Class<?> clazz,
      final PreparedStatementCreator psc,
      final KeyHolder generatedKeyHolder)
      throws DataAccessException {
    return aroundWrite(clazz, () -> jdbcTemplate.update(psc, generatedKeyHolder));
  }

  @Override
  public int[] batchUpdate(
      @Nonnull final Class<?> clazz, String sql, final BatchPreparedStatementSetter pss)
      throws DataAccessException {
    return aroundWrite(clazz, () -> jdbcTemplate.batchUpdate(sql, pss));
  }

  @Override
  public int update(
      @Nonnull final Class<?> clazz, String sql, @Nullable PreparedStatementSetter pss)
      throws DataAccessException {
    return aroundWrite(clazz, () -> jdbcTemplate.update(sql, pss));
  }

  private <T> T aroundRead(@Nonnull final SqlParam<?> sqlParam, Supplier<T> supplier) {
    return supplier.get();
  }

  private <T> T aroundWrite(@Nonnull final Class<?> clazz, Supplier<T> supplier) {
    return supplier.get();
  }
}
