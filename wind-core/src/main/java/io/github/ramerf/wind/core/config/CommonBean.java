package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.cache.*;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.executor.JdbcTemplateExecutor;
import io.github.ramerf.wind.core.factory.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.serializer.JacksonEnumSerializer;
import io.github.ramerf.wind.core.support.*;
import io.github.ramerf.wind.core.util.EnvironmentUtil;
import java.util.LinkedHashSet;
import java.util.Set;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.annotation.*;
import org.springframework.core.Ordered;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 定义常用bean.
 *
 * @author Tang Xiaofeng
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
   * String to enum converter factory mvc configure web mvc configurer.
   *
   * @return the web mvc configurer
   */
  @Bean
  @ConditionalOnProperty(
      value = "wind.enable-web-mvc-configurer",
      havingValue = "true",
      matchIfMissing = true)
  public WebMvcConfigurer stringToEnumConverterFactoryMvcConfigure() {
    // 添加枚举转换器,请求可以传递value整型值
    return new WebMvcConfigurer() {
      @Override
      public void addFormatters(@Nonnull FormatterRegistry registry) {
        registry.addConverterFactory(new StringToEnumConverterFactory());
      }

      @Override
      public void addCorsMappings(@Nonnull CorsRegistry registry) {
        // TODO-WARN 这个跨域配置有问题
        final long maxAge = 3600L;
        registry
            .addMapping("/**")
            .allowedOrigins("*")
            .allowedMethods("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "TRACE")
            .allowCredentials(false)
            .maxAge(maxAge);
      }
    };
  }

  /**
   * Jackson object mapper customizer jackson 2 object mapper builder customizer.
   *
   * @return the jackson 2 object mapper builder customizer
   */
  @Bean
  public Jackson2ObjectMapperBuilderCustomizer jacksonObjectMapperCustomizer() {
    return objectMapperBuilder ->
        objectMapperBuilder.serializerByType(InterEnum.class, new JacksonEnumSerializer());
  }

  /**
   * Default redis cache redis cache.
   *
   * @return the redis cache
   */
  @Bean
  @ConditionalOnMissingBean(RedisCache.class)
  @DependsOn("redisCacheRedisTemplate")
  @ConditionalOnProperty(value = "wind.redis-cache.enable", havingValue = "true")
  public Cache defaultRedisCache(WindConfiguration configuration) {
    return new DefaultRedisCache(configuration);
  }

  /**
   * Default redis cache redis cache.
   *
   * @return the redis cache
   */
  @Bean
  @ConditionalOnMissingBean(Cache.class)
  @ConditionalOnProperty(value = "wind.redis-cache.enable", havingValue = "false")
  public Cache inMemoryCache(WindConfiguration configuration) {
    return new InMemoryCache(configuration);
  }

  /**
   * Jdbc template executor executor.
   *
   * @return the executor
   */
  @Bean
  public Executor jdbcTemplateExecutor(ObjectProvider<RedisCache> redisCache) {
    return new JdbcTemplateExecutor(redisCache.getIfAvailable());
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

  /**
   * Environment util environment util.
   *
   * @return the environment util
   */
  @Bean
  public EnvironmentUtil environmentUtil() {
    return new EnvironmentUtil();
  }
}
