package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
@Slf4j
public class DefaultRedisCache implements RedisCache {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Resource private WindConfiguration windConfiguration;

  @Override
  public String getKeyPrefix(@Nonnull final Class<?> clazz) {
    return windConfiguration.getRedisCache().getKeyPrefix();
  }

  @Override
  public void put(final String key, final Object value) {
    getOperations().set(key, value, 10, TimeUnit.MINUTES);
  }

  @Override
  public void put(String key, Object value, long timeout, TimeUnit timeUnit) {
    getOperations().set(key, value, timeout, timeUnit);
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

  @Override
  public boolean isKeyExist(@Nonnull final String key) {
    final Boolean hasKey = redisTemplate.hasKey(key);
    return Objects.nonNull(hasKey) && hasKey;
  }

  private ValueOperations<String, Object> getOperations() {
    return redisTemplate.opsForValue();
  }
}
