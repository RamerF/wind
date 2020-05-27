package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.cache.RedisCache;
import java.util.*;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

/**
 * The jdbc template executor.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/19
 */
@Slf4j
@Component
public class JdbcTemplateExecutor implements Executor {
  @Resource private JdbcTemplate jdbcTemplate;

  @Autowired(required = false)
  private RedisCache redisCache;

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
    return cacheIfAbsent(sqlParam, () -> jdbcTemplate.query(sqlParam.sql, pss, rse));
  }

  @Override
  public <T> List<T> query(
      @Nonnull final SqlParam sqlParam,
      final PreparedStatementSetter pss,
      final RowMapper<T> rowMapper)
      throws DataAccessException {
    return cacheIfAbsent(sqlParam, () -> jdbcTemplate.query(sqlParam.sql, pss, rowMapper));
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
    if (Objects.nonNull(exist)) {
      if (log.isDebugEnabled()) {
        log.debug("cacheIfAbsent:Hit cache[{}]", key);
      }
      return (T) exist;
    }
    final T t = supplier.get();
    if (log.isDebugEnabled()) {
      log.debug("cacheIfAbsent:Put cache[{}]", key);
    }
    redisCache.put(key, t);
    return t;
  }

  private <T> T execAndClear(@Nonnull final Class<?> clazz, Supplier<T> supplier) {
    // 未开启缓存
    if (Objects.isNull(redisCache)) {
      return supplier.get();
    }
    final String key = redisCache.getKeyPrefix(clazz) + clazz.getName();
    if (log.isDebugEnabled()) {
      log.debug("execAndClear:Clear cache[{}]", key);
    }
    redisCache.clear(key);
    return supplier.get();
  }
}
