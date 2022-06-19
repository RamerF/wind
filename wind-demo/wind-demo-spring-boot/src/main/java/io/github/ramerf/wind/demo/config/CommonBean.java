package io.github.ramerf.wind.demo.config;

import io.github.ramerf.wind.spring.boot.autoconfigure.WindProperty;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 通用bean定义.
 *
 * @since 2022.04.09
 * @author ramer
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(WindProperty.class)
public class CommonBean {

  @Bean
  @ConfigurationProperties(prefix = "spring.datasource.hikari")
  public DataSource dataSource() {
    return DataSourceBuilder.create().build();
  }
}
