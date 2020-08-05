package io.github.ramerf.wind.core.executor;

import com.alibaba.fastjson.JSON;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.ramerf.wind.core.cache.RedisCache;
import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.*;
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

  private final RedisCache redisCache;

  public JdbcTemplateExecutor(RedisCache redisCache) {
    this.redisCache = redisCache;
  }

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
        },
        Thread.currentThread().getStackTrace()[1].getMethodName());
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
        },
        Thread.currentThread().getStackTrace()[1].getMethodName());
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
        },
        Thread.currentThread().getStackTrace()[1].getMethodName());
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
          return PageUtils.toPage(
              resultHandler.handle(list), total, currentPage, pageSize, pageable.getSort());
        },
        Thread.currentThread().getStackTrace()[1].getMethodName());
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
                }),
        Thread.currentThread().getStackTrace()[1].getMethodName());
  }

  @Override
  public <T> T queryForObject(
      @Nonnull final SqlParam sqlParam, final Object[] args, final Class<T> requiredType)
      throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> jdbcTemplate.queryForObject(sqlParam.sql, args, requiredType),
        Thread.currentThread().getStackTrace()[1].getMethodName());
  }

  @Override
  public List<Map<String, Object>> queryForList(
      @Nonnull final SqlParam sqlParam, final Object... args) throws DataAccessException {
    return cacheIfAbsent(
        sqlParam,
        () -> jdbcTemplate.queryForList(sqlParam.sql, args),
        Thread.currentThread().getStackTrace()[1].getMethodName());
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
  private <T> T cacheIfAbsent(
      @Nonnull final SqlParam sqlParam, Supplier<T> supplier, final String methodName) {
    // 未开启缓存
    if (Objects.isNull(redisCache) || sqlParam.clazz == null) {
      return supplier.get();
    }
    final String key = redisCache.generateKey(sqlParam, methodName);
    if (redisCache.isKeyExist(key)) {
      if (log.isDebugEnabled()) {
        log.debug("cacheIfAbsent:Hit cache[{}]", key);
      }
      final Object exist = redisCache.get(key);
      if (exist instanceof RedisPageImpl) {
        return (T) ((RedisPageImpl<?>) exist).getPage();
      } else {
        return (T) exist;
      }
    }
    final T t = supplier.get();
    if (log.isDebugEnabled()) {
      log.debug("cacheIfAbsent:Put cache[{}]", key);
    }
    // 空数据缓存100ms,防止穿透数据库,这个数值可能应该允许让用户自定义
    if (Objects.isNull(t)) {
      redisCache.put(key, null, 100, TimeUnit.MILLISECONDS);
    } else {
      if (t instanceof Page) {
        // redisCache.put(key, new RedisPageImpl<>((Page<?>) t));
        try {
          redisCache.put(key, new RedisPageImpl<>(new ObjectMapper().writeValueAsString(t)));
        } catch (JsonProcessingException e) {
          e.printStackTrace();
        }
      } else {
        redisCache.put(key, t);
      }
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

  @Setter
  @NoArgsConstructor
  public static class RedisPageImpl<T> {
    // private List<T> content;
    // // private Pageable pageable;
    // private List<SortColumn> sortColumns;
    // private long total;
    // private int page;
    // private int size;
    // private Sort sort;

    // public RedisPageImpl(final Page<T> page) {
    //   this.content = page.getContent();
    //   // this.pageable = page.getPageable();
    //   this.total = page.getTotalElements();
    //   this.page = page.getPageable().getPageNumber();
    //   this.size = page.getPageable().getPageSize();
    //   this.sort = page.getPageable().getSort();
    // }

    private String json;

    public RedisPageImpl(final String json) {
      this.json = json;
    }

    @JsonIgnore
    public Page<T> getPage() {
      try {
        return new ObjectMapper().readValue(json, Page.class);
      } catch (IOException e) {
        e.printStackTrace();
      }
      return null;
    }
    // @JsonIgnore
    // public Page<T> getPage() {
    //   return PageUtils.toPage(content, total, page, size, sort);
    // }
  }
}
