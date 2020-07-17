package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.cache.RedisCache;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.util.*;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;

/**
 * The jdbc template executor.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/19
 */
@Slf4j
public class JdbcTemplateExecutor implements Executor {
  @Resource private JdbcTemplate jdbcTemplate;

  @Autowired(required = false)
  @SuppressWarnings("SpringJavaAutowiredFieldsWarningInspection")
  private RedisCache redisCache;

  @Override
  @SuppressWarnings("unchecked")
  public <R> R fetchOne(@Nonnull final SqlParam sqlParam) throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> {
          final List<Map<String, Object>> result =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.conditions.stream()
                          .flatMap(condition -> condition.getValues(sqlParam.startIndex).stream())
                          .forEach(o -> o.accept(ps)),
                  new ColumnMapRowMapper());
          if (CollectionUtils.isEmpty(result)) {
            return null;
          }
          if (result.size() > 1) {
            throw CommonException.of(ResultCode.API_TOO_MANY_RESULTS);
          }
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          ResultHandler<Map<String, Object>, R> resultHandler =
              BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
                  ? new PrimitiveResultHandler<>(clazz)
                  : new BeanResultHandler<>(clazz, sqlParam.queryColumns);
          return resultHandler.handle(result.get(0));
        });
  }

  @Override
  public <R> List<R> fetchAll(@Nonnull final SqlParam sqlParam, final Class<R> clazz)
      throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> {
          final List<Map<String, Object>> list =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.conditions.stream()
                          .flatMap(condition -> condition.getValues(sqlParam.startIndex).stream())
                          .forEach(o -> o.accept(ps)),
                  new ColumnMapRowMapper());
          if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
          }
          ResultHandler<Map<String, Object>, R> resultHandler =
              BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
                  ? new PrimitiveResultHandler<>(clazz)
                  : new BeanResultHandler<>(clazz, sqlParam.queryColumns);
          return resultHandler.handle(list);
        });
  }

  @Override
  @SuppressWarnings("unchecked")
  public <R> List<R> fetchAll(@Nonnull final SqlParam sqlParam) throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> {
          final List<Map<String, Object>> list =
              query(
                  sqlParam,
                  ps ->
                      sqlParam.conditions.stream()
                          .flatMap(condition -> condition.getValues(sqlParam.startIndex).stream())
                          .forEach(o -> o.accept(ps)),
                  new ColumnMapRowMapper());
          if (log.isDebugEnabled()) {
            log.debug("fetch:[{}]", list);
          }
          final Class<R> clazz = (Class<R>) sqlParam.getClazz();
          ResultHandler<Map<String, Object>, R> resultHandler =
              BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
                  ? new PrimitiveResultHandler<>(clazz)
                  : new BeanResultHandler<>(clazz, sqlParam.queryColumns);
          return resultHandler.handle(list);
        });
  }

  @Override
  public <R> Page<R> fetchPage(
      @Nonnull final SqlParam sqlParam, final long total, final PageRequest pageable)
      throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> {
          final List<Map<String, Object>> list =
              total < 1
                  ? null
                  : query(
                      sqlParam,
                      ps ->
                          sqlParam.conditions.stream()
                              .flatMap(
                                  condition -> condition.getValues(sqlParam.startIndex).stream())
                              .forEach(o -> o.accept(ps)),
                      new ColumnMapRowMapper());
          // 从0开始
          final int currentPage = pageable.getPageNumber();
          // 每页大小
          final int pageSize = pageable.getPageSize();
          if (CollectionUtils.isEmpty(list)) {
            return PageUtils.toPage(Collections.emptyList(), 0, currentPage, pageSize);
          }
          final Class<?> clazz = sqlParam.getClazz();
          ResultHandler resultHandler =
              BeanUtils.isPrimitiveType(clazz) || clazz.isArray()
                  ? new PrimitiveResultHandler<>(clazz)
                  : new BeanResultHandler<>(clazz, sqlParam.queryColumns);
          return PageUtils.toPage(resultHandler.handle(list), total, currentPage, pageSize);
        });
  }

  @Override
  public long fetchCount(@Nonnull final SqlParam sqlParam) {
    return cacheIfAbsent(
        sqlParam,
        () ->
            query(
                sqlParam,
                ps ->
                    sqlParam.conditions.stream()
                        .flatMap(condition -> condition.getValues(sqlParam.startIndex).stream())
                        .forEach(o -> o.accept(ps)),
                rs -> {
                  if (rs.next()) {
                    return rs.getLong(1);
                  }
                  return 0L;
                }));
  }

  @Override
  public <T> T queryForObject(
      @Nonnull final SqlParam sqlParam, final Object[] args, final Class<T> requiredType)
      throws DataAccessException {
    return cacheIfAbsent(
        sqlParam, () -> jdbcTemplate.queryForObject(sqlParam.sql, args, requiredType));
  }

  @Override
  public List<Map<String, Object>> queryForList(
      @Nonnull final SqlParam sqlParam, final Object... args) throws DataAccessException {
    return cacheIfAbsent(sqlParam, () -> jdbcTemplate.queryForList(sqlParam.sql, args));
  }

  @Override
  public <T> T query(
      @Nonnull final SqlParam sqlParam,
      final PreparedStatementSetter pss,
      final ResultSetExtractor<T> rse)
      throws DataAccessException {
    return jdbcTemplate.query(sqlParam.sql, pss, rse);
  }

  @Override
  public <T> List<T> query(
      @Nonnull final SqlParam sqlParam,
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
    return execAndClear(clazz, () -> jdbcTemplate.update(psc, generatedKeyHolder));
  }

  @Override
  public int[] batchUpdate(
      @Nonnull final Class<?> clazz, String sql, final BatchPreparedStatementSetter pss)
      throws DataAccessException {
    return execAndClear(clazz, () -> jdbcTemplate.batchUpdate(sql, pss));
  }

  @Override
  public int update(
      @Nonnull final Class<?> clazz, String sql, @Nullable PreparedStatementSetter pss)
      throws DataAccessException {
    return execAndClear(clazz, () -> jdbcTemplate.update(sql, pss));
  }

  @SuppressWarnings("unchecked")
  private <T> T cacheIfAbsent(@Nonnull final SqlParam sqlParam, Supplier<T> supplier) {
    // 未开启缓存
    if (Objects.isNull(redisCache)) {
      return supplier.get();
    }
    final String key = redisCache.generateKey(sqlParam);
    final Object exist = redisCache.get(key);
    if (redisCache.isKeyExist(key)) {
      if (log.isDebugEnabled()) {
        log.debug("cacheIfAbsent:Hit cache[{}]", key);
      }
      return (T) exist;
    }
    final T t = supplier.get();
    if (log.isDebugEnabled()) {
      log.debug("cacheIfAbsent:Put cache[{}]", key);
    }
    // 空数据缓存100ms,防止穿透数据库,这个数值可能应该允许让用户自定义
    if (Objects.isNull(t)) {
      redisCache.put(key, null, 100, TimeUnit.MILLISECONDS);
    } else {
      redisCache.put(key, t);
    }
    return t;
  }

  private <T> T execAndClear(@Nonnull final Class<?> clazz, Supplier<T> supplier) {
    // 未开启缓存
    if (Objects.isNull(redisCache)) {
      return supplier.get();
    }
    redisCache.clear(clazz);
    return supplier.get();
  }
}
