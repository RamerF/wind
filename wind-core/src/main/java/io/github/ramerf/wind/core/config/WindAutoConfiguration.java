package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Mybatis turbo 初始化配置.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/28
 */
@Slf4j
@Configuration
public class WindAutoConfiguration implements ApplicationContextAware {

  @Bean
  @ConditionalOnMissingBean
  public TypeConverterRegistryFactory typeConverterRegistryFactory() {
    /* 这里可以任意扩展,加自定义转换器,读配置文件等. */
    return new TypeConverterRegistryFactory();
  }

  @Override
  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    final WindProperty windProperty = applicationContext.getBean(WindProperty.class);
    // 初始化分布式主键
    SnowflakeIdWorker.setWorkerId(windProperty.getWorkId());
    SnowflakeIdWorker.setDatacenterId(windProperty.getDataCenterId());
    // 初始化实体类
    applicationContext.getBeansWithAnnotation(SpringBootApplication.class).values().stream()
        .findFirst()
        .map(Object::getClass)
        .map(o -> o.getAnnotation(SpringBootApplication.class))
        .ifPresent(o -> initEntityInfo(o, windProperty));
  }

  private void initEntityInfo(
      final SpringBootApplication application, final WindProperty property) {
    String parent = String.join(",", application.scanBasePackages());
    final String entityPackage =
        StringUtils.nonEmpty(parent) ? parent : property.getEntityPackage();
    log.info("initEntityInfo:init entity info[{}]", entityPackage);
    try {
      BeanUtils.scanClasses(entityPackage, AbstractEntityPoJo.class)
          .forEach(EntityHelper::initEntity);
    } catch (IOException e) {
      log.warn("initEntityInfo:fail to init entity info[{}]", e.getMessage());
    }
  }
}
