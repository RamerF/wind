package io.github.ramerf.wind.core.autoconfig;

import io.github.ramerf.wind.core.annotation.ConfigurationProperties;
import io.github.ramerf.wind.core.annotation.NestedConfigurationProperties;
import io.github.ramerf.wind.core.autoconfig.jdbc.DataSourceConfigurationFactory;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import io.github.ramerf.wind.core.util.YmlUtil.After;
import io.github.ramerf.wind.core.util.YmlUtil.YmlAfter;
import java.util.Map;
import lombok.Data;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * 读取文件自动配置{@link Configuration}.
 *
 * @since 2022.03.05
 * @author ramer
 */
@Data
@Slf4j
@ConfigurationProperties(prefix = "wind")
public class AutoConfigConfiguration {

  /** 逻辑删除配置. */
  @NestedConfigurationProperties private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** entity所在包路径,多个以,分割.<br> */
  private String entityPackage = "";

  /** 拦截器所在包路径,多个以,分割. */
  protected String interceptorPackage = "";

  /** 类型处理器路径,多个以,分割 */
  protected String typeHandlerPackage = "";

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 500;

  /** 表更新模式. */
  private DdlAuto ddlAuto = DdlAuto.NONE;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  private boolean writeNullProp = true;

  /** 全局id生成器,类全路径,默认自增 {@link IdGenerator#AUTO_INCREMENT_ID_GENERATOR } */
  private String idGenerator;

  @NestedConfigurationProperties private DataSourceConfig dataSource;

  @Data
  public static class DataSourceConfig {
    /** 事务工厂. */
    private Class<? extends TransactionFactory> transactionFactory = JdbcTransactionFactory.class;

    /** 数据库方言全路径. */
    private String dialect;

    /** 定义数据源属性,根据不同的数据源使用不同的属性 */
    @Getter private Map<String, String> properties;

    /** 默认支持的数据源. */
    public enum DataSourceType {
      DBCP,
      HIKARI,
      DRUID,
      OTHER;
    }

    @After
    public void after(YmlAfter ymlAfter) {
      this.properties = ymlAfter.getProperties();
    }
  }

  public Configuration getConfiguration() {
    Configuration configuration = new Configuration();
    configuration.setLogicDeleteProp(logicDeleteProp);
    configuration.setEntityPackage(entityPackage);
    configuration.setInterceptorPackage(interceptorPackage);
    configuration.setTypeHandlerPackage(typeHandlerPackage);
    configuration.setBatchSize(batchSize);
    configuration.setDdlAuto(ddlAuto);
    configuration.setWriteNullProp(writeNullProp);
    if (StringUtils.hasText(idGenerator)) {
      try {
        final IdGenerator idGenerator = BeanUtils.initial(this.idGenerator);
        configuration.setIdGenerator(idGenerator);
      } catch (WindException e) {
        log.error(String.format("Cannot initial idGenerator [%s]", idGenerator), e.getCause());
        throw e;
      }
    }
    if (dataSource != null) {
      configuration.setDialect(dataSource.getDialect());
      configuration.setJdbcEnvironment(
          new JdbcEnvironment(
              BeanUtils.initial(dataSource.getTransactionFactory()),
              DataSourceConfigurationFactory.getDataSource(dataSource)));
    }
    return configuration;
  }
}
