package io.github.ramerf.wind.core.config;

import lombok.Data;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;
import org.springframework.context.annotation.Configuration;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
@Data
@Configuration
@ConfigurationProperties("wind")
public class WindConfiguration {
  private String logicDeleteField = "isDelete";

  private boolean logicNotDelete = false;

  private boolean logicDeleted = true;
  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String entityPackage = "";

  @NestedConfigurationProperty private SnowflakeProp snowflakeProp = new SnowflakeProp();
}
