package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.DependsOn;
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
@DependsOn("redisCacheRedisTemplate")
public class DefaultRedisCache implements RedisCache {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Override
  public String getKeyPrefix(@Nonnull final Class<?> clazz) {
    return "io.github.ramerf.wind:";
  }

  @Override
  public void put(final String key, final Object value) {
    getOperations().set(key, value, 10, TimeUnit.MINUTES);
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
