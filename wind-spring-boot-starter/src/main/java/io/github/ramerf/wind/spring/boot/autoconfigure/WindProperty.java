package io.github.ramerf.wind.spring.boot.autoconfigure;

import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

/**
 * @author ramer
 * @since 2022.06.03
 */
@Data
@Slf4j
@org.springframework.context.annotation.Configuration
@ConfigurationProperties(prefix = "wind")
public class WindProperty {
  /** 逻辑删除配置. */
  @NestedConfigurationProperty private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

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

  public Configuration getConfiguration() {
    Configuration configuration = new Configuration();
    configuration.setLogicDeleteProp(logicDeleteProp);
    configuration.setEntityPackage(entityPackage);
    configuration.setInterceptorPackage(interceptorPackage);
    configuration.setTypeHandlerPackage(typeHandlerPackage);
    configuration.setBatchSize(batchSize);
    configuration.setDdlAuto(ddlAuto);
    configuration.setWriteNullProp(writeNullProp);
    return configuration;
  }
}
