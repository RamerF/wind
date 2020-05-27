package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.helper.SqlHelper;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
public interface RedisCache {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(RedisCache.class);

  /**
   * 自定义key前缀.
   *
   * @param clazz the pojo
   * @return the string
   */
  default String getKeyPrefix(@Nonnull final Class<?> clazz) {
    return "";
  }

  /**
   * 放入redis.
   *
   * @param key the key
   * @param value the value
   */
  void put(String key, Object value);

  /**
   * 获取对象.
   *
   * @param key the key
   * @return the object
   */
  Object get(String key);

  /**
   * 清除对应key的数据.
   *
   * @param key the key
   */
  void clear(@Nonnull String key);

  /**
   * 指定缓存key前缀.
   *
   * @param sqlParam the sql param
   * @return the string
   * @see SqlParam
   */
  default String generateKey(@Nonnull final SqlParam sqlParam) {
    return getKeyPrefix(sqlParam.getClazz())
        + sqlParam.getClazz().getName()
        + ":"
        + sqlParam.getSql()
        + ":"
        + (Objects.nonNull(sqlParam.getAggregateFunction())
            ? sqlParam.getAggregateFunction().name()
            : sqlParam.getConditions().stream()
                .flatMap(o -> o.getOriginValues().stream())
                .map(SqlHelper::toSqlString)
                .collect(Collectors.joining()));
  }
}
