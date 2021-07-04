package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.*;
import org.springframework.data.redis.serializer.RedisSerializer;

/**
 * 用于查询redis缓存.
 *
 * @author ramer
 * @since 2020 /5/26
 */
@Slf4j
public class RedisCache extends AbstractCache {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  public RedisCache(final WindConfiguration configuration) {
    super(configuration);
  }

  @Override
  public void put(final String key, final Object value) {
    // getOperations().set(key, value, 10, TimeUnit.MINUTES);
    getOperations().set(key, value);
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
    CollectionUtils.doIfNonEmpty(scan(key + "*"), o -> o.forEach(k -> redisTemplate.delete(k)));
  }

  @Override
  public boolean isKeyExist(@Nonnull final String key) {
    return scan(key + "*").size() > 0;
  }

  private ValueOperations<String, Object> getOperations() {
    return redisTemplate.opsForValue();
  }

  private List<String> scan(String pattern) {
    ScanOptions options = ScanOptions.scanOptions().match(pattern).count(Integer.MAX_VALUE).build();
    @SuppressWarnings("unchecked")
    RedisSerializer<String> keySerializer =
        (RedisSerializer<String>) redisTemplate.getKeySerializer();
    try (final ConvertingCursor<byte[], String> cursor =
        redisTemplate.executeWithStickyConnection(
            connection ->
                new ConvertingCursor<>(connection.scan(options), keySerializer::deserialize))) {
      if (cursor == null) {
        return Collections.emptyList();
      }
      List<String> keys = new ArrayList<>();
      while (cursor.hasNext()) {
        keys.add(cursor.next());
      }
      return keys;
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
