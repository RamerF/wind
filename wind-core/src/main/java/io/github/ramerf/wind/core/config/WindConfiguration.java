package io.github.ramerf.wind.core.config;

import lombok.Data;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;
import org.springframework.context.annotation.Configuration;

/**
 * The type Wind configuration.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/14
 */
@Data
@Configuration
@ConfigurationProperties("wind")
public class WindConfiguration {
  /** 逻辑删除字段. */
  private String logicDeleteField = "isDelete";

  /** 逻辑未删除值. */
  private boolean logicNotDelete = false;

  /** 逻辑已删除值. */
  private boolean logicDeleted = true;

  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String entityPackage = "";

  /**
   * 枚举所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String enumPackage = "";

  /**是否自定义*/
  private boolean customEnumDeserializer = false;

  /** 批量操作时,每次处理的大小. */
  private int batchSize = 150;

  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();
}
