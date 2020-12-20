package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.WindConfiguration;
import java.util.*;
import java.util.concurrent.TimeUnit;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
@Slf4j
public class InMemoryCache extends AbstractCache {
  private final Map<String, InMemoryCacheObj> cacheMap =
      Collections.synchronizedMap(new WeakHashMap<>());

  public InMemoryCache(final WindConfiguration configuration) {
    super(configuration);
  }

  @Override
  public void put(final String key, final Object value) {
    cacheMap.put(key, InMemoryCacheObj.of(key, value));
  }

  @Override
  public void put(String key, Object value, long timeout, TimeUnit timeUnit) {
    cacheMap.put(key, InMemoryCacheObj.of(key, value, timeout, timeUnit));
  }

  @Override
  public Object get(final String key) {
    final InMemoryCacheObj exist = cacheMap.get(key);
    if (exist == null) {
      return null;
    }
    if (exist.notExpired()) {
      return exist.object;
    }
    clear(key);
    return null;
  }

  @Override
  public void clear(@Nonnull final String key) {
    cacheMap.entrySet().removeIf(entry -> entry.getKey().startsWith(key));
  }

  @Override
  public boolean isKeyExist(@Nonnull final String key) {
    return cacheMap.keySet().stream().anyMatch(o -> o.startsWith(key));
  }

  @Override
  public String toString() {
    return "InMemoryCache{" + "cacheMap=" + cacheMap + '}';
  }

  private static class InMemoryCacheObj extends Observable {
    private String key;
    private Object object;
    private long timeout;
    private TimeUnit timeUnit;
    private Date expireTime;

    public static InMemoryCacheObj of(
        final String key, final Object object, final long timeout, final TimeUnit timeUnit) {
      final InMemoryCacheObj obj = new InMemoryCacheObj();
      obj.key = key;
      obj.object = object;
      obj.timeout = timeout;
      obj.timeUnit = timeUnit;
      obj.expireTime = new Date(timeUnit.toMillis(timeout) + System.currentTimeMillis());
      return obj;
    }

    public static InMemoryCacheObj of(final String key, final Object object) {
      final InMemoryCacheObj obj = new InMemoryCacheObj();
      obj.key = key;
      obj.object = object;
      return obj;
    }

    /** 是否已过期. */
    public boolean notExpired() {
      return expireTime == null || expireTime.after(new Date());
    }
  }
}
