package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.executor.JdbcTemplateExecutor;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.serializer.InterEnumSerializer;
import io.github.ramerf.wind.core.support.IdGenerator;
import java.util.LinkedHashSet;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
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

  /** 注册类型转换器. */
  @Bean
  public TypeHandlerRegistryFactory typeHandlerRegistryFactory() {
    final TypeHandlerRegistryFactory factory = new TypeHandlerRegistryFactory();
    factory.addTypeHandlers(typeHandlers);
    factory.registerDefaultTypeHandlers();
    return factory;
  }

  @Bean
  @ConditionalOnMissingBean
  public Executor jdbcTemplateExecutor() {
    return new JdbcTemplateExecutor();
  }

  /** id默认自增. */
  @Bean
  @ConditionalOnMissingBean(IdGenerator.class)
  public IdGenerator autoIncrementIdGenerator() {
    return o -> null;
  }

  /** 枚举默认使用value方法序列化. */
  @Bean
  @ConditionalOnMissingBean(InterEnumSerializer.class)
  public InterEnumSerializer interEnumSerializer() {
    return InterEnum::value;
  }
}
