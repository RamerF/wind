package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.cache.*;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.executor.JdbcTemplateExecutor;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.support.SnowflakeIdGenerator;
import java.util.LinkedHashSet;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.*;
import org.springframework.core.Ordered;
import org.springframework.data.redis.core.RedisTemplate;

/**
 * 定义常用bean.
 *
 * @author ramer
 * @since 2019 /12/29
 */
@Slf4j
@Configuration("wind_core_common_bean")
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE + 10)
@AutoConfigureAfter({RedisTemplate.class, RedisConfiguration.class, RedisCacheConfiguration.class})
public class CommonBean {
  @Autowired(required = false)
  @SuppressWarnings({"rawtypes", "SpringJavaAutowiredFieldsWarningInspection"})
  private final Set<ITypeHandler> typeHandlers = new LinkedHashSet<>();

  /**
   * Type handler registry factory type handler registry factory.
   *
   * @return the type handler registry factory
   */
  @Bean
  public TypeHandlerRegistryFactory typeHandlerRegistryFactory() {
    final TypeHandlerRegistryFactory factory = new TypeHandlerRegistryFactory();
    factory.addTypeHandlers(typeHandlers);
    factory.registerDefaultTypeHandlers();
    return factory;
  }

  /**
   * Default redis cache redis cache.
   *
   * @return the redis cache
   */
  @Bean
  @ConditionalOnMissingBean(Cache.class)
  @DependsOn("redisCacheRedisTemplate")
  @ConditionalOnProperty(value = "wind.cache.type", havingValue = "redis")
  public Cache defaultRedisCache(WindConfiguration configuration) {
    return new RedisCache(configuration);
  }

  /**
   * In memory cache cache.
   *
   * @param configuration the configuration
   * @return the redis cache
   */
  @Bean
  @ConditionalOnMissingBean(Cache.class)
  @ConditionalOnProperty(value = "wind.cache.type", havingValue = "memory")
  public Cache inMemoryCache(WindConfiguration configuration) {
    return new InMemoryCache(configuration);
  }

  /**
   * Jdbc template executor executor.
   *
   * @return the executor
   */
  @Bean
  public Executor jdbcTemplateExecutor(ObjectProvider<Cache> cacheObjectProvider) {
    return new JdbcTemplateExecutor(cacheObjectProvider.getIfAvailable());
  }

  /**
   * Snowflake id generator id generator.
   *
   * @return the id generator
   */
  @Bean
  @ConditionalOnMissingBean(IdGenerator.class)
  public IdGenerator snowflakeIdGenerator() {
    return new SnowflakeIdGenerator();
  }
}
