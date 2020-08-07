package io.github.ramerf.wind.core.config;

import java.util.stream.Stream;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.*;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext.SerializationPair;

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
@EnableCaching
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE)
@AutoConfigureAfter(name = "redisCacheRedisTemplate")
@ConditionalOnBean(name = "redisCacheRedisTemplate")
public class RedisConfiguration {
  @Resource(name = "redisCacheRedisTemplate")
  private RedisTemplate<String, Object> redisTemplate;

  /** 缓存key生成策略.用法:{@code @Cacheable(value="xxx", keyGenerator="keyGenerator")} */
  @Bean
  public KeyGenerator keyGenerator() {
    return (target, method, params) -> {
      StringBuilder sb = new StringBuilder();
      sb.append(target.getClass().getName()).append(":").append(method.getName());
      Stream.of(params).forEach(sb::append);
      return sb.toString();
    };
  }

  @Bean
  @ConditionalOnMissingBean
  @SuppressWarnings({"unchecked", "rawtypes"})
  public CacheManager cacheManager(LettuceConnectionFactory connectionFactory) {
    // spring cache注解序列化配置
    final SerializationPair keySerializationPair =
        SerializationPair.fromSerializer(redisTemplate.getKeySerializer());
    final SerializationPair valueSerializationPair =
        SerializationPair.fromSerializer(redisTemplate.getValueSerializer());
    RedisCacheConfiguration redisCacheConfiguration =
        RedisCacheConfiguration.defaultCacheConfig()
            .serializeKeysWith(keySerializationPair)
            .serializeValuesWith(valueSerializationPair)
        // .disableCachingNullValues()
        // 配置全局失效时间
        // .entryTtl(Duration.ofSeconds(60))
        ;
    return RedisCacheManager.builder(connectionFactory)
        .cacheDefaults(redisCacheConfiguration)
        .transactionAware()
        .build();
  }
}
