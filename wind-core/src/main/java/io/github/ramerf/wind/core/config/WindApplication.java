package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.WindVersion;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.ansi.*;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration;
import io.github.ramerf.wind.core.autoconfig.AutoConfigConfiguration.DataSourceConfig;
import io.github.ramerf.wind.core.exception.ClassInstantiationException;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.ioc.ApplicationContext;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Properties;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class WindApplication {
  private static final WindContext windContext = new WindContext();
  private static ApplicationContext applicationContext;

  private WindApplication() {}

  /** 通过指定配置文件启动. */
  public static void refresh(final String configPath) {
    final AutoConfigConfiguration autoConfigConfiguration =
        YmlUtil.process(AutoConfigConfiguration.class, configPath);
    final Configuration configuration = autoConfigConfiguration.getConfiguration();
    final DataSourceConfig dataSourceConfig = autoConfigConfiguration.getDataSourceConfig();
    final String driverClassName = dataSourceConfig.getDriverClassName();
    Asserts.hasText(driverClassName, "Need to specify driverClassName in dataSourceConfig");
    final DataSource dataSource;
    try {
      dataSource = BeanUtils.initial(driverClassName);
    } catch (ClassInstantiationException e) {
      throw new IllegalStateException("Cannot initial class " + driverClassName);
    }
    // TODO WARN 可配置
    TransactionFactory transactionFactory = new JdbcTransactionFactory();
    configuration.setJdbcEnvironment(new JdbcEnvironment(transactionFactory, dataSource));
    refresh(configuration);
  }

  public static void refresh(@Nonnull final DataSource dataSource) {
    refresh(new JdbcTransactionFactory(), dataSource);
  }

  public static void refresh(
      @Nonnull final TransactionFactory transactionFactory, @Nonnull final DataSource dataSource) {
    Configuration configuration = new Configuration();
    configuration.setJdbcEnvironment(new JdbcEnvironment(transactionFactory, dataSource));
    refresh(configuration);
  }

  public static void refresh(@Nonnull Configuration configuration) {
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    Asserts.notNull(jdbcEnvironment, "需要指定数据源");
    final DataSource dataSource = jdbcEnvironment.getDataSource();
    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, configuration.getDialect()));
    windContext.setConfiguration(configuration);
    windContext.setExecutor(new SimpleJdbcExecutor(dataSource));
    try {
      afterPropertiesSet();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext) {
    WindApplication.applicationContext = applicationContext;
  }

  private TransactionFactory getTransactionFactory(@Nullable final String type, Properties props) {
    TransactionFactory factory = BeanUtils.initial(type);
    factory.setProperties(props);
    return factory;
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
    initEntityInfo(windContext.getConfiguration());
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

  private static void initEntityInfo(final Configuration configuration) {
    String scanBasePackages;
    String entityPackage;
    if (StringUtils.nonEmpty(configuration.getEntityPackage())) {
      entityPackage = configuration.getEntityPackage();
    } else {
      entityPackage = WindVersion.class.getPackage().getName();
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
      log.warn("initEntityInfo:fail to init entity info[{}]", e.getMessage());
      return;
    }
    if (entities != null) {
      entities.forEach(EntityHelper::initEntity);
      EntityHelper.initEntityMapping();
    }
  }
}
