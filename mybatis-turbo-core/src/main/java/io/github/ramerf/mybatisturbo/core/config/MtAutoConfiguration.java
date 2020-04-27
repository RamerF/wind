package io.github.ramerf.mybatisturbo.core.config;

import io.github.ramerf.mybatisturbo.core.factory.TypeConverterRegistryFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Mybatis turbo 初始化配置.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/28
 */
@Configuration
public class MtAutoConfiguration implements InitializingBean {

  @Bean
  @ConditionalOnMissingBean
  public TypeConverterRegistryFactory typeConverterRegistryFactory() {
    /* 这里可以任意扩展,加自定义转换器,读配置文件等. */
    return new TypeConverterRegistryFactory();
  }

  @Override
  public void afterPropertiesSet() throws Exception {}
}
