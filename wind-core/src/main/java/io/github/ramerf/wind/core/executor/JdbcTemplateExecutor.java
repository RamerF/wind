package io.github.ramerf.wind.core.executor;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.github.ramerf.wind.core.cache.Cache;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.*;
import io.github.ramerf.wind.core.util.*;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.data.domain.*;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.jdbc.core.*;
import org.springframework.jdbc.support.KeyHolder;
import org.springframework.lang.Nullable;

import static java.util.stream.Collectors.toList;

/**
 * The jdbc template executor.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/19
 */
@Slf4j
@SuppressWarnings("DuplicatedCode")
public class JdbcTemplateExecutor implements Executor {
  @Resource @Getter private JdbcTemplate jdbcTemplate;

  private final Cache cache;

  public JdbcTemplateExecutor(Cache cache) {
    this.cache = cache;
  }

  @Override
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
          @SuppressWarnings("unchecked")
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
  @SuppressWarnings("unchecked")
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
          @SuppressWarnings("rawtypes")
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
  @SuppressWarnings("ConstantConditions")
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
    if (Objects.isNull(cache)) {
      return supplier.get();
    }
    final String key = cache.generateKey(sqlParam, methodName);
    // 命中缓存
    if (cache.isKeyExist(key)) {
      if (log.isDebugEnabled()) {
        log.debug("cacheIfAbsent:Hit cache[{}]", key);
      }
      final Object exist = cache.get(key);
      if (exist == null) {
        return null;
      }
      if (exist instanceof PageInRedis) {
        return (T) ((PageInRedis<?>) exist).getPage();
      }
      // 由于redis没有Long型,存入的Long取出可能会失败
      if (exist instanceof Number && Long.class.isAssignableFrom(sqlParam.clazz)) {
        return (T) Long.valueOf(exist.toString());
      }
      return (T) exist;
    }
    // 没有缓存
    final T t = supplier.get();
    if (log.isDebugEnabled()) {
      log.debug("cacheIfAbsent:Put cache[{}]", key);
    }
    // 空数据缓存50ms,防止穿透数据库,这个数值可能应该允许让用户自定义
    if (Objects.isNull(t)) {
      cache.put(key, null, 50, TimeUnit.MILLISECONDS);
    } else {
      // Page对象需要单独处理
      final Object putRedis = t instanceof Page ? new PageInRedis<>((Page<?>) t) : t;
      cache.put(key, putRedis);
    }
    return t;
  }

  private <T> T execAndClear(@Nonnull final Class<?> clazz, Supplier<T> supplier) {
    // 未开启缓存
    if (Objects.isNull(cache)) {
      return supplier.get();
    }
    cache.clear(clazz);
    return supplier.get();
  }

  /**
   * 由于{@link PageImpl}无法序列化,写了这一大堆,真让人头大😔.
   *
   * @param <T>
   */
  @Setter
  @NoArgsConstructor
  private static class PageInRedis<T> {
    private int page;
    private int size;
    private long total;
    private List<T> content;
    private List<Order> orders;

    public PageInRedis(final Page<T> page) {
      this.page = page.getPageable().getPageNumber();
      this.size = page.getPageable().getPageSize();
      this.total = page.getTotalElements();
      this.content = page.getContent();
      this.orders =
          page.getPageable().getSort().stream()
              .map(o -> Order.of(o.getDirection(), o.getProperty()))
              .collect(toList());
    }

    @JsonIgnore
    public Page<T> getPage() {
      final Sort sort =
          Sort.by(
              orders.stream()
                  .map(
                      o ->
                          o.getDirection() == Direction.ASC
                              ? Sort.Order.asc(o.property)
                              : Sort.Order.desc(o.property))
                  .collect(toList()));
      return PageUtils.toPage(content, total, page, size, sort);
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor(staticName = "of")
    private static class Order {
      private Direction direction;
      private String property;
    }
  }
}
