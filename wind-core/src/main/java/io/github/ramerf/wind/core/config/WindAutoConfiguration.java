package io.github.ramerf.wind.core.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.github.ramerf.wind.core.WindVersion;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.event.InitFinishEvent;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.serializer.JacksonEnumDeserializer;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.support.SnowflakeIdWorker;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.ansi.*;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.*;
import org.springframework.context.annotation.Configuration;

/**
 * 初始化配置.
 *
 * @author ramer
 * @since 2020.3.28
 */
@Slf4j
@Configuration
@ConditionalOnBean(DataSource.class)
@EnableConfigurationProperties(WindConfiguration.class)
@AutoConfigureAfter({CommonBean.class, PrototypeBean.class})
public class WindAutoConfiguration implements ApplicationContextAware, InitializingBean {
  private final WindConfiguration configuration;
  private final WindContext windContext = new WindContext();
  private ApplicationContext applicationContext;
  private final ApplicationEventPublisher publisher;
  private final ObjectMapper objectMapper;
  private final Executor executor;
  private final IdGenerator idGenerator;
  private final PrototypeBean prototypeBean;

  public WindAutoConfiguration(
      final WindConfiguration windConfiguration,
      final DataSource dataSource,
      final ApplicationEventPublisher publisher,
      final ObjectMapper objectMapper,
      final Executor executor,
      final ObjectProvider<IdGenerator> idGenerator,
      final PrototypeBean prototypeBean) {
    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, windConfiguration.getDialect()));
    windContext.setWindConfiguration(windConfiguration);

    this.configuration = windConfiguration;
    this.publisher = publisher;
    this.objectMapper = objectMapper;
    this.executor = executor;
    this.idGenerator = idGenerator.getIfAvailable();
    this.prototypeBean = prototypeBean;
  }

  @Override
  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    this.applicationContext = applicationContext;
  }

  @Override
  @SuppressWarnings("RedundantThrows")
  public void afterPropertiesSet() throws Exception {
    // 打印banner
    printBanner();
    windContext.setExecutor(executor);
    AppContextInject.initital(applicationContext);
    // 初始化分布式主键
    SnowflakeIdWorker.initial(configuration.getSnowflakeProp());
    // 初始化Query/Update
    Update.initial(
        executor,
        configuration,
        idGenerator,
        windContext.getDbMetaData().getDialect(),
        prototypeBean);
    Query.initial(executor, configuration, prototypeBean);
    // 初始化EntityUtils
    EntityUtils.initial(windContext);
    // 初始化实体类
    final Class<?> bootClass =
        applicationContext.getBeansWithAnnotation(SpringBootApplication.class).values().stream()
            .findFirst()
            .map(Object::getClass)
            .orElseThrow(
                () -> new IllegalStateException("No class annotate with @SpringBootApplication."));
    // 初始化实体信息
    EntityHelper.initital(windContext);
    initEntityInfo(bootClass, configuration);
    // 初始化枚举反序列化器
    registerEnumDeserializer(bootClass, configuration);
    // 发布初始化完成事件
    publisher.publishEvent(new InitFinishEvent(windContext));
  }

  private void printBanner() {
    System.out.println(
        "\n"
            + " _       __    ____    _   __    ____ \n"
            + "| |     / /   /  _/   / | / /   / __ \\\n"
            + "| | /| / /    / /    /  |/ /   / / / /\n"
            + "| |/ |/ /   _/ /_   / /|  /   / /_/ /\n"
            + "|__/|__/   /___/   /_/ |_/   /_____/");
    System.out.println(
        AnsiOutput.toString(
            AnsiColor.GREEN,
            " :: wind ::",
            AnsiColor.DEFAULT,
            " (v" + WindVersion.getVersion() + ")\n",
            AnsiStyle.FAINT));
  }

  private void initEntityInfo(final Class<?> bootClass, final WindConfiguration configuration) {
    final SpringBootApplication application = bootClass.getAnnotation(SpringBootApplication.class);
    String scanBasePackages;
    String entityPackage;
    if (StringUtils.nonEmpty(configuration.getEntityPackage())) {
      entityPackage = configuration.getEntityPackage();
    } else if (Objects.nonNull(application)
        && StringUtils.nonEmpty(
            scanBasePackages = String.join(",", application.scanBasePackages()))) {
      entityPackage = scanBasePackages;
    } else {
      entityPackage = bootClass.getPackage().getName();
    }
    log.info("initEntityInfo:package[{}]", entityPackage);
    Set<Class<?>> entities;
    try {
      entities = BeanUtils.scanClassesWithAnnotation(entityPackage, TableInfo.class);
      if (entities.size() < 1) {
        log.info(
            "no entity with @TableInfo annotation found in path: [{}], correct your configuration:wind.entity-package",
            entityPackage);
      }
    } catch (IOException e) {
      entities = null;
      log.warn("initEntityInfo:fail to init entity info[{}]", e.getMessage());
    }
    if (entities != null) {
      entities.forEach(EntityHelper::initEntity);
      EntityHelper.initEntityMapping();
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
