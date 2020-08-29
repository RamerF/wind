package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
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
   * 获取key前缀,如果不为空,加上:后缀
   *
   * @param clazz 操作的pojo
   * @return 包含 :的key前缀
   */
  default String getFixedKeyPrefix(@Nonnull final Class<?> clazz) {
    String keyPrefix = getKeyPrefix(clazz);
    // 如果结尾没有包含冒号,加上
    return StringUtils.isEmpty(keyPrefix)
        ? ""
        : keyPrefix.endsWith(":") ? keyPrefix : keyPrefix + ":";
  }

  /**
   * 放入redis.
   *
   * @param key the key
   * @param value the value
   */
  void put(String key, Object value);

  /**
   * 放入redis.
   *
   * @param key the key
   * @param value the value
   * @param timeout 超时时间
   * @param timeUnit 超时时间单位
   */
  void put(String key, Object value, long timeout, TimeUnit timeUnit);

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
   * 清除对应key的数据.
   *
   * @param clazz {@link AbstractEntityPoJo}
   */
  default void clear(@Nonnull final Class<?> clazz) {
    final String key = getFixedKeyPrefix(clazz) + clazz.getName();
    if (log.isDebugEnabled()) {
      log.debug("clear:Clear cache[{}]", key);
    }
    if (isKeyExist(key)) {
      clear(key);
    }
  }

  /**
   * 判断给定的key是否存在.true:存在.
   *
   * @param key the key
   * @return the boolean
   */
  boolean isKeyExist(@Nonnull final String key);

  /**
   * 指定缓存key前缀.
   *
   * @param sqlParam the sql param
   * @param methodName the method name
   * @return the string
   * @see SqlParam
   */
  default String generateKey(@Nonnull final SqlParam sqlParam, @Nonnull final String methodName) {
    return getFixedKeyPrefix(sqlParam.getClazz())
        + sqlParam.getClazz().getName()
        + ":"
        + methodName
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
