package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.WindVersion;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.ansi.*;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.ioc.ApplicationContext;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;

/**
 * TODO WARN 初始化框架.
 *
 * @author ramer
 * @since 2020.3.28
 */
@Slf4j
public class WindApplication {
  private static final WindContext windContext = new WindContext();
  private static ApplicationContext applicationContext;

  private WindApplication() {}

  public static void run(
      @Nullable Configuration configuration, @Nonnull final DataSource dataSource) {
    if (configuration == null) {
      configuration = new Configuration();
    }
    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, configuration.getDialect()));
    windContext.setConfiguration(configuration);
    windContext.setExecutor(new SimpleJdbcExecutor(dataSource));
    try {
      afterPropertiesSet();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    WindApplication.applicationContext = applicationContext;
  }

  @SuppressWarnings("RedundantThrows")
  private static void afterPropertiesSet() throws Exception {
    // 打印banner
    printBanner();
    AppContextInject.initial(applicationContext);
    // 初始化Query/Update
    Update.initial(windContext.getExecutor(), windContext.getConfiguration());
    Query.initial(windContext.getExecutor(), windContext.getConfiguration());
    // 初始化EntityUtils
    EntityUtils.initial(windContext);
    // 初始化实体信息
    EntityHelper.initital(windContext);
    initEntityInfo(WindVersion.class, windContext.getConfiguration());
    // TODO WARN 发布初始化完成事件
    //   publisher.publishEvent(new InitFinishEvent(windContext));
  }

  private static void printBanner() {
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

  private static void initEntityInfo(final Class<?> bootClass, final Configuration configuration) {
    String scanBasePackages;
    String entityPackage;
    if (StringUtils.nonEmpty(configuration.getEntityPackage())) {
      entityPackage = configuration.getEntityPackage();
    } else {
      entityPackage = bootClass.getPackage().getName();
    }
    log.info("initEntityInfo:package[{}]", entityPackage);
    ApplicationContext applicationContext =
        new ApplicationContext(entityPackage + "," + WindVersion.class.getPackage().getName());
    AppContextInject.initial(applicationContext);
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
}
