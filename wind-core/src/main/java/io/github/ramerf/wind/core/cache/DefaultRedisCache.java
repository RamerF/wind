package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.RedisCacheConfiguration;
import io.github.ramerf.wind.core.util.CollectionUtils;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.stereotype.Component;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
@Slf4j
@Component
@ConditionalOnBean(RedisCacheConfiguration.class)
public class DefaultRedisCache implements RedisCache {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Override
  public String getKeyPrefix(@Nonnull final Class<?> clazz) {
    return "io.github.ramerf.wind:";
  }

  @Override
  public void put(final String key, final Object value) {
    getOperations().set(key, value);
  }

  @Override
  public Object get(final String key) {
    return getOperations().get(key);
  }

  @Override
  public void clear(@Nonnull final String key) {
    CollectionUtils.doIfNonEmpty(
        redisTemplate.keys(key + "*"), o -> o.forEach(k -> redisTemplate.delete(k)));
  }

  private ValueOperations<String, Object> getOperations() {
    return redisTemplate.opsForValue();
  }
}
