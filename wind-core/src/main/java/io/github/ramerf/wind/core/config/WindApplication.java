package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.WindVersion;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.ansi.*;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WindApplication {
  private static final WindContext windContext = new WindContext();

  private WindApplication() {}

  /** 通过指定配置文件启动. */
  public static void run(final String configPath) {
    final AutoConfigConfiguration autoConfigConfiguration =
        YmlUtil.process(AutoConfigConfiguration.class, configPath);
    run(autoConfigConfiguration.getConfiguration());
  }

  public static void run(@Nonnull final DataSource dataSource) {
    run(new JdbcTransactionFactory(), dataSource);
  }

  public static void run(
      @Nonnull final TransactionFactory transactionFactory, @Nonnull final DataSource dataSource) {
    Configuration configuration = new Configuration();
    configuration.setJdbcEnvironment(new JdbcEnvironment(transactionFactory, dataSource));
    run(configuration);
  }

  public static void run(@Nonnull Configuration configuration) {
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    Asserts.notNull(jdbcEnvironment, "需要指定数据源");
    final DataSource dataSource = jdbcEnvironment.getDataSource();
    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, configuration.getDialect()));
    windContext.setConfiguration(configuration);
    try {
      // 打印banner
      printBanner();
      // 初始化Query/Update
      Update.initial(windContext);
      Query.initial(windContext);
      // 初始化实体解析类
      EntityUtils.initial(windContext);
      EntityHelper.initial(windContext);
      // 解析实体元数据
      initEntityInfo(windContext.getConfiguration());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static WindContext getWindContext() {
    return windContext;
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

  private static void initEntityInfo(final Configuration configuration) {
    String scanBasePackages;
    String entityPackage;
    if (StringUtils.nonEmpty(configuration.getEntityPackage())) {
      entityPackage = configuration.getEntityPackage();
    } else {
      entityPackage = WindVersion.class.getPackage().getName();
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
      log.warn("initEntityInfo:fail to init entity info[{}]", e.getMessage());
      return;
    }
    if (!entities.isEmpty()) {
      entities.forEach(EntityHelper::initEntity);
      EntityHelper.initEntityMapping();
    }
  }
}
