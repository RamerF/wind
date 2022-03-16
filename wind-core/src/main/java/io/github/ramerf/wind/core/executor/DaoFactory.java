package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.WindVersion;
import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.ansi.*;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.exception.ClassInstantiationException;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.metadata.DbMetaData;
import io.github.ramerf.wind.core.plugin.Interceptor;
import io.github.ramerf.wind.core.util.*;
import java.io.IOException;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
public class DaoFactory {
  private Configuration configuration;

  public <T> Query<T> getQuery(final Class<T> clazz) {
    return new Query<>(configuration, clazz);
  }

  public <T> Update<T> getUpdate(final Class<T> clazz) {
    return new Update<>(clazz);
  }

  private static final WindContext windContext = new WindContext();

  public static DaoFactory newInstance(@Nonnull final DataSource dataSource) {
    return newInstance(new JdbcTransactionFactory(), dataSource);
  }

  public static DaoFactory newInstance(
      @Nonnull final TransactionFactory transactionFactory, @Nonnull final DataSource dataSource) {
    Configuration configuration = new Configuration();
    configuration.setJdbcEnvironment(new JdbcEnvironment(transactionFactory, dataSource));
    return newInstance(configuration);
  }

  public static DaoFactory newInstance(@Nonnull Configuration configuration) {
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    Asserts.notNull(jdbcEnvironment, "需要指定数据源");
    final DataSource dataSource = jdbcEnvironment.getDataSource();
    windContext.setDbMetaData(DbMetaData.getInstance(dataSource, configuration.getDialect()));
    windContext.setConfiguration(configuration);
    // 设置拦截器
    populateInterceptors(configuration);
    // 打印banner
    printBanner();
    // 初始化Query/Update
    Update.initial(windContext);
    // 初始化实体解析类
    EntityUtils.initial(windContext);
    EntityHelper.initial(windContext);
    // 解析实体元数据
    initEntityInfo(windContext.getConfiguration());
  }

  private static void populateInterceptors(final Configuration configuration) {
    final String interceptorPackage = configuration.getInterceptorPackage();
    if (StringUtils.nonEmpty(interceptorPackage)) {
      try {
        final Set<Class<? extends Interceptor>> classes =
            BeanUtils.scanClasses(interceptorPackage, Interceptor.class);
        for (Class<? extends Interceptor> clazz : classes) {
          try {
            configuration.addInterceptor(BeanUtils.initial(clazz));
          } catch (ClassInstantiationException e) {
            throw new WindException(
                "Fail to initial interceptor:" + clazz + ",require no arg constructor", e);
          }
        }
      } catch (IOException e) {
        log.warn("Fail to populate interceptors:" + interceptorPackage, e);
      }
    }
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
      log.warn("Fail to init entity info:" + entityPackage, e);
      return;
    }
    if (!entities.isEmpty()) {
      entities.forEach(EntityHelper::initEntity);
      EntityHelper.initEntityMapping();
    }
  }
}
