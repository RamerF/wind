package io.github.ramerf.wind.spring.boot.autoconfigure;

import io.github.ramerf.wind.core.annotation.UpdateTimestamp;
import io.github.ramerf.wind.core.config.Configuration.TimestampStrategy;
import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

import static io.github.ramerf.wind.spring.boot.autoconfigure.WindProperty.WIND_PROPERTY_PREFIX;

/**
 * @author ramer
 * @since 2022.06.03
 */
@Data
@Slf4j
@org.springframework.context.annotation.Configuration
@ConfigurationProperties(prefix = WIND_PROPERTY_PREFIX)
public class WindProperty {
  public static final String WIND_PROPERTY_PREFIX = "wind";
  /** 逻辑删除配置. */
  @NestedConfigurationProperty private LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** entity所在包路径,多个以,分割.<br> */
  private String entityPackage = "";

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 500;

  /** 表更新模式. */
  private DdlAuto ddlAuto = DdlAuto.NONE;

  /** 新增/更新时写入值为null的属性,默认写入所有字段. */
  private boolean writeNullProp = true;

  /** 指定{@link UpdateTimestamp}注解的更新策略,默认总是赋值为当前时间 */
  protected TimestampStrategy updateTimeStrategy = TimestampStrategy.ALWAYS;

  public Configuration getConfiguration() {
    Configuration configuration = new Configuration();
    configuration.setLogicDeleteProp(logicDeleteProp);
    configuration.setEntityPackage(entityPackage);
    configuration.setBatchSize(batchSize);
    configuration.setDdlAuto(ddlAuto);
    configuration.setWriteNullProp(writeNullProp);
    configuration.setUpdateTimeStrategy(updateTimeStrategy);
    return configuration;
  }
}
