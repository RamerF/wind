package io.github.ramerf.wind.core.cache;

import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
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
// @ConditionalOnProperty(name = "spring.redis.host")
// @ConditionalOnMissingBean(RedisCache.class)
public class DefaultRedisCache implements RedisCache {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  @Override
  public void put(final String key, final Object value) {
    log.info("put:[key:{},value:{}]", key, value);
    getOperations().set(key, value);
  }

  @Override
  public Object get(final String key) {
    log.info("get:[{}]", key);
    return getOperations().get(key);
  }

  @Override
  public void clear(final String key) {
    log.info("clear:[{}]", key);
    redisTemplate.delete(key);
  }

  private ValueOperations<String, Object> getOperations() {
    return redisTemplate.opsForValue();
  }
}
