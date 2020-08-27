package io.github.ramerf.wind.core.config;

import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.Resource;
import org.springframework.jdbc.datasource.init.DataSourceInitializer;
import org.springframework.jdbc.datasource.init.ResourceDatabasePopulator;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020.08.27
 */
// @Configuration
public class DatabaseInitialize {
  @Value("classpath:db-test.sql")
  private Resource dbResource;

  @Bean
  public DataSourceInitializer dataSourceInitializer(DataSource dataSource) {
    final DataSourceInitializer initializer = new DataSourceInitializer();
    initializer.setDataSource(dataSource);
    final ResourceDatabasePopulator populator = new ResourceDatabasePopulator();
    populator.addScripts(dbResource);
    initializer.setDatabasePopulator(populator);
    return initializer;
  }
}
