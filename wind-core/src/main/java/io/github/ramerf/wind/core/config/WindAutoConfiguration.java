package io.github.ramerf.wind.core.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import io.github.ramerf.wind.core.WindVersion;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.event.InitFinishEvent;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.serializer.JacksonEnumDeserializer;
import io.github.ramerf.wind.core.support.SnowflakeIdWorker;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.ansi.*;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.*;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.Assert;

/**
 * 初始化配置.
 *
 * @author Tang Xiaofeng
 * @since 2020 /3/28
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(WindConfiguration.class)
@AutoConfigureAfter({CommonBean.class, PrototypeBean.class})
public class WindAutoConfiguration implements ApplicationContextAware, InitializingBean {
  private final WindConfiguration windConfiguration;
  private final WindContext windContext = new WindContext();

  private ApplicationContext applicationContext;
  @Resource private ObjectMapper objectMapper;
  private final ApplicationEventPublisher publisher;

  public WindAutoConfiguration(
      final WindConfiguration windConfiguration,
      @Qualifier("dataSource") final DataSource dataSource,
      final ApplicationEventPublisher publisher) {
    this.windConfiguration = windConfiguration;

    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, windConfiguration.getDialect()));
    windContext.setWindConfiguration(windConfiguration);

    this.publisher = publisher;
  }

  @Override
  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    this.applicationContext = applicationContext;
  }

  @Override
  public void afterPropertiesSet() throws Exception {
    // 打印banner
    printBanner();
    // 初始化分布式主键
    SnowflakeIdWorker.setWorkerId(windConfiguration.getSnowflakeProp().getWorkerId());
    SnowflakeIdWorker.setDatacenterId(windConfiguration.getSnowflakeProp().getDataCenterId());
    AppContextInject.context = applicationContext;
    // 初始化Query/Update
    Query.executor = Update.executor = AppContextInject.getBean(JdbcTemplateExecutor.class);
    windContext.setJdbcTemplateExecutor(Query.executor);
    // 初始化实体类
    final Class<?> bootClass =
        applicationContext.getBeansWithAnnotation(SpringBootApplication.class).values().stream()
            .findFirst()
            .map(Object::getClass)
            .orElse(null);
    Assert.notNull(bootClass, "No class annotate with @SpringBootApplication.");
    initEntityInfo(bootClass, windConfiguration);
    // 初始化枚举反序列化器
    registerEnumDeserializer(bootClass, windConfiguration);
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
    EntityHelper.initital(windContext);
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
