package io.github.ramerf.wind.core.autoconfig;

import com.alibaba.druid.pool.DruidDataSource;
import io.github.ramerf.wind.core.annotation.ConfigurationProperties;
import io.github.ramerf.wind.core.annotation.NestedConfigurationProperties;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import javax.sql.DataSource;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * 读取文件自动配置{@link Configuration}.
 *
 * @since 2020 /1/14
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

  /** 枚举所在包路径,多个以,分割.<br> */
  private String enumPackage = "";

  /** 是否自定义枚举反序列化.设置为true时,可能需要编写枚举反序列化代码. */
  private boolean customEnumDeserializer = false;

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 150;

  /** 是否启用默认mvc配置. */
  private boolean enableWebMvcConfigurer = true;

  /** 表更新模式. */
  private DdlAuto ddlAuto = DdlAuto.NONE;

  /** 数据库方言全路径. */
  private String dialect;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  private boolean writeNullProp = true;

  /** 全局id生成器,类全路径,默认自增 {@link IdGenerator#AUTO_INCREMENT_ID_GENERATOR } */
  private String idGenerator;

  @NestedConfigurationProperties private DataSourceConfig dataSourceConfig;

  @Data
  public static class DataSourceConfig {
    /** 数据源提供者的全路径,默认使用{@link DruidDataSource} */
    private Class<? extends DataSource> type = DruidDataSource.class;

    private String url;
    private String username;
    private String password;
    private String driverClassName;

    public enum DataSourceType {
      DBCP,
      HIKARI,
      DRUID,
      ;
    }
  }

  public Configuration getConfiguration() {
    Configuration configuration = new Configuration();
    configuration.setLogicDeleteProp(logicDeleteProp);
    configuration.setEntityPackage(entityPackage);
    configuration.setEnumPackage(enumPackage);
    configuration.setCustomEnumDeserializer(customEnumDeserializer);
    configuration.setBatchSize(batchSize);
    configuration.setEnableWebMvcConfigurer(enableWebMvcConfigurer);
    configuration.setDdlAuto(ddlAuto);
    configuration.setDialect(dialect);
    configuration.setWriteNullProp(writeNullProp);
    if (StringUtils.hasText(idGenerator)) {
      try {
        final IdGenerator idGenerator = BeanUtils.initial(this.idGenerator);
        configuration.setIdGenerator(idGenerator);
      } catch (CommonException e) {
        log.error(String.format("Cannot initial idGenerator [%s]", idGenerator), e.getCause());
        throw e;
      }
    }
    return configuration;
  }
}
