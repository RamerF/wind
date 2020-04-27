package io.github.ramerf.mybatisturbo.core.config;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.*;
import io.github.ramerf.mybatisturbo.core.util.CollectionUtils;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.cache.Cache;
import org.springframework.data.redis.connection.RedisServerCommands;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.*;

@Slf4j
public class RedisCache implements Cache {
  private final ReadWriteLock readWriteLock = new ReentrantReadWriteLock(true);

  private static RedisTemplate<String, Object> redisTemplate;

  public static void setRedisTemplate(final RedisTemplate<String, Object> redisTemplate) {
    RedisCache.redisTemplate = redisTemplate;
  }

  private final String id;

  public RedisCache(final String id) {
    if (id == null) {
      throw new IllegalArgumentException("Cache instances require an ID");
    }
    this.id = id;
  }

  @Override
  public String getId() {
    return this.id;
  }

  @Override
  public void putObject(Object key, Object value) {
    initRedisTemplate();
    if (value != null) {
      redisTemplate.opsForValue().set(key.toString(), value);
    }
  }

  @Override
  public Object getObject(Object key) {
    initRedisTemplate();
    try {
      if (key != null) {
        log.info("getObject:[=====Using cache=====]");
        return redisTemplate.opsForValue().get(key.toString());
      }
    } catch (Exception e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return null;
  }

  @Override
  public Object removeObject(Object key) {
    if (key != null) {
      redisTemplate.delete(key.toString());
    }
    return null;
  }

  @Override
  public void clear() {
    initRedisTemplate();
    Set<String> keys = redisTemplate.keys("*:" + this.id + "*");
    if (!CollectionUtils.isEmpty(keys)) {
      redisTemplate.delete(keys);
    }
  }

  @Override
  public int getSize() {
    Long size = redisTemplate.execute(RedisServerCommands::dbSize);
    return Objects.nonNull(size) ? size.intValue() : 0;
  }

  @Override
  public ReadWriteLock getReadWriteLock() {
    return this.readWriteLock;
  }

  /** 如果全局注入redisTemplate失败,执行该方法初始化. */
  private void initRedisTemplate() {
    if (redisTemplate == null) {
      redisTemplate = AppContextInject.getBean("redisTemplate");
      RedisSerializer<String> redisSerializer = new StringRedisSerializer();
      Jackson2JsonRedisSerializer<?> jackson2JsonRedisSerializer =
          new Jackson2JsonRedisSerializer<>(Object.class);
      ObjectMapper objectMapper = new ObjectMapper();
      objectMapper.setVisibility(PropertyAccessor.ALL, JsonAutoDetect.Visibility.ANY);
      objectMapper.enableDefaultTyping(ObjectMapper.DefaultTyping.NON_FINAL);
      objectMapper.setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE);
      objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

      jackson2JsonRedisSerializer.setObjectMapper(objectMapper);
      redisTemplate.setKeySerializer(redisSerializer);
      redisTemplate.setValueSerializer(jackson2JsonRedisSerializer);
      redisTemplate.setHashValueSerializer(jackson2JsonRedisSerializer);
    }
  }
}
