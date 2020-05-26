package io.github.ramerf.wind.core.cache;

import java.lang.reflect.Method;
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
  void clear(String key);

  /**
   * 指定缓存key前缀.
   *
   * @param target the target
   * @param method the method
   * @param params the params
   * @return the string
   */
  default String generateKey(
      @Nonnull final Object target, @Nonnull final Method method, @Nonnull Object... params) {
    log.info("generateKey:[target:{},method:{},params:{}]", target, method.getName(), params);
    return null;
  }
}
