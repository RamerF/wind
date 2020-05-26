package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.exception.CommonException;
import java.time.Duration;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.cache.annotation.*;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;

/**
 * 自定义Spring redis cache.
 *
 * @author Tang Xiaofeng
 * @since 2020/03/24
 * @see Cacheable
 * @see CacheEvict
 * @see EnableCaching
 */
@Slf4j
@Configuration
@AutoConfigureAfter(value = RedisTemplate.class)
@EnableCaching
public class RedisConfiguration extends CachingConfigurerSupport {

  // @Autowired
  // private RedisConnectionFactory redisConnectionFactory;

  @Bean
  @Override
  public KeyGenerator keyGenerator() {
    // 在没有指定缓存Key的情况下，key生成策略
    return (target, method, params) -> {
      StringBuilder sb = new StringBuilder();
      sb.append(target.getClass().getName()).append("::").append(method.getName());
      Stream.of(params).forEach(param -> sb.append(param.toString()));
      return sb.toString();
    };
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Bean
  public RedisCacheManager redisCacheManager(RedisTemplate redisTemplate) {
    // spring cache注解序列化配置
    RedisCacheConfiguration redisCacheConfiguration =
        RedisCacheConfiguration.defaultCacheConfig()
            .serializeKeysWith(
                RedisSerializationContext.SerializationPair.fromSerializer(
                    redisTemplate.getKeySerializer()))
            .serializeValuesWith(
                RedisSerializationContext.SerializationPair.fromSerializer(
                    redisTemplate.getValueSerializer()))
            // .disableCachingNullValues()
            .entryTtl(Duration.ofSeconds(60));
    return RedisCacheManager.builder(
            CommonException.requireNonNull(
                redisTemplate.getConnectionFactory(), "fail to init redisTemplate."))
        .cacheDefaults(redisCacheConfiguration)
        .transactionAware()
        .build();
  }
}
