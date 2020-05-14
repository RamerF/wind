package io.github.ramerf.wind.core.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
@Setter
@Getter
@Configuration
@ConfigurationProperties("wind")
public class WindProperty {
  /**
   * entity所在包路径,多个以,分割.<br>
   * 如果没有配置该值,使用{@link SpringBootApplication#scanBasePackages()}
   */
  private String entityPackage;
}
