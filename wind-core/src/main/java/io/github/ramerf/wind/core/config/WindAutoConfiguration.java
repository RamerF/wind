package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;
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
 * 初始化配置.
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
    final WindConfiguration configuration = applicationContext.getBean(WindConfiguration.class);
    // 初始化分布式主键
    SnowflakeIdWorker.setWorkerId(configuration.getSnowflakeProp().getWorkId());
    SnowflakeIdWorker.setDatacenterId(configuration.getSnowflakeProp().getDataCenterId());
    // 初始化实体类
    applicationContext.getBeansWithAnnotation(SpringBootApplication.class).values().stream()
        .findFirst()
        .map(Object::getClass)
        .ifPresent(o -> initEntityInfo(o, configuration));
  }

  private void initEntityInfo(final Class<?> clazz, final WindConfiguration configuration) {
    final SpringBootApplication application = clazz.getAnnotation(SpringBootApplication.class);
    String scanBasePackages;
    String entityPackage;
    if (StringUtils.nonEmpty(configuration.getEntityPackage())) {
      entityPackage = configuration.getEntityPackage();
    } else if (Objects.nonNull(application)
        && StringUtils.nonEmpty(
            scanBasePackages = String.join(",", application.scanBasePackages()))) {
      entityPackage = scanBasePackages;
    } else {
      entityPackage = clazz.getPackage().getName();
    }
    log.info("initEntityInfo:init entity info[{}]", entityPackage);
    try {
      final Set<Class<? extends AbstractEntityPoJo>> entities =
          BeanUtils.scanClasses(entityPackage, AbstractEntityPoJo.class);
      if (entities.size() < 1) {
        log.error(
            String.format(
                "no entity with @Entity annotation found in path: %s, correct your configuration:wind.entity-package",
                entityPackage));
      }
      // 下面这行确保查询指定公共列时lambda可以使用AbstractEntityPoJo指定.如: AbstractEntityPoJo::getId
      entities.add(AbstractEntityPoJo.class);
      entities.forEach(EntityHelper::initEntity);
    } catch (IOException e) {
      log.warn("initEntityInfo:fail to init entity info[{}]", e.getMessage());
    }
  }
}
