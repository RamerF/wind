package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
public interface Cache {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(Cache.class);

  WindConfiguration getConfiguration();

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
   * 清除以keyPrefix为前缀对应的数据.
   *
   * @param keyPrefix key 前缀
   */
  void clear(@Nonnull String keyPrefix);

  /**
   * 清除对应key的数据.
   *
   * @param clazz {@link AbstractEntityPoJo}
   */
  void clear(@Nonnull final Class<?> clazz);

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
  String generateKey(@Nonnull final SqlParam sqlParam, @Nonnull final String methodName);
}
