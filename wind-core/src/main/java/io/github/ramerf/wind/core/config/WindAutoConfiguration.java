package io.github.ramerf.wind.core.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.github.ramerf.wind.core.converter.TypeConverter;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.serializer.JacksonEnumDeserializer;
import io.github.ramerf.wind.core.serializer.JacksonEnumSerializer;
import io.github.ramerf.wind.core.support.SnowflakeIdWorker;
import io.github.ramerf.wind.core.support.StringToEnumConverterFactory;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.IOException;
import java.util.*;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.format.FormatterRegistry;
import org.springframework.util.Assert;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * 初始化配置.
 *
 * @author Tang Xiaofeng
 * @since 2020 /3/28
 */
@Slf4j
@Configuration
public class WindAutoConfiguration implements ApplicationContextAware {
  @Resource private ObjectMapper objectMapper;

  @Autowired(required = false)
  @SuppressWarnings({"rawtypes", "SpringJavaAutowiredFieldsWarningInspection"})
  private final Set<TypeConverter> typeConverters = new LinkedHashSet<>();

  /**
   * Type converter registry factory type converter registry factory.
   *
   * @return the type converter registry factory
   */
  @Bean
  public TypeConverterRegistryFactory typeConverterRegistryFactory() {
    final TypeConverterRegistryFactory factory = new TypeConverterRegistryFactory();
    factory.addTypeConverter(typeConverters);
    factory.registerDefaultTypeConverters();
    return factory;
  }

  /**
   * String to enum converter factory mvc configure web mvc configurer.
   *
   * @return the web mvc configurer
   */
  @Bean
  public WebMvcConfigurer stringToEnumConverterFactoryMvcConfigure() {
    // 添加枚举转换器,请求可以传递value整型值
    return new WebMvcConfigurer() {
      @Override
      public void addFormatters(@Nonnull FormatterRegistry registry) {
        registry.addConverterFactory(new StringToEnumConverterFactory());
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

  @Override
  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    final WindConfiguration configuration = applicationContext.getBean(WindConfiguration.class);
    // 初始化分布式主键
    SnowflakeIdWorker.setWorkerId(configuration.getSnowflakeProp().getWorkerId());
    SnowflakeIdWorker.setDatacenterId(configuration.getSnowflakeProp().getDataCenterId());
    // 初始化Query/Update
    Query.executor = Update.executor = AppContextInject.getBean(JdbcTemplateExecutor.class);
    // 初始化实体类
    final Class<?> bootClass =
        applicationContext.getBeansWithAnnotation(SpringBootApplication.class).values().stream()
            .findFirst()
            .map(Object::getClass)
            .orElse(null);
    Assert.notNull(bootClass, "No class annotate with @SpringBootApplication.");
    initEntityInfo(bootClass, configuration);
    // 初始化枚举反序列化器
    registerEnumDeserializer(bootClass, configuration);
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

  @SuppressWarnings({"unchecked", "rawtypes"})
  private void registerEnumDeserializer(
      final Class<?> clazz, final WindConfiguration configuration) {
    if (configuration.isCustomEnumDeserializer()) {
      return;
    }
    // 注册默认枚举反序列化器
    final SpringBootApplication application = clazz.getAnnotation(SpringBootApplication.class);
    String scanBasePackages;
    String enumPackage;
    if (StringUtils.nonEmpty(configuration.getEnumPackage())) {
      enumPackage = configuration.getEnumPackage();
    } else if (Objects.nonNull(application)
        && StringUtils.nonEmpty(
            scanBasePackages = String.join(",", application.scanBasePackages()))) {
      enumPackage = scanBasePackages;
    } else {
      enumPackage = clazz.getPackage().getName();
    }
    log.info("registerEnumDeserializer:register enum deserializer[{}]", enumPackage);
    try {
      final Set<Class<? extends InterEnum>> classes =
          BeanUtils.scanClasses(enumPackage, InterEnum.class);
      classes.forEach(
          o -> {
            SimpleModule module = new SimpleModule();
            module.addDeserializer(o, new JacksonEnumDeserializer(o));
            objectMapper.registerModule(module);
          });
    } catch (IOException e) {
      log.warn("initEntityInfo:fail to register enum deserializer[{}]", e.getMessage());
    }
  }
}
