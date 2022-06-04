package io.github.ramerf.wind.spring.boot.autoconfigure;

import io.github.ramerf.wind.WindApplication;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.DaoFactory;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.jdbc.transaction.TransactionFactory;
import io.github.ramerf.wind.core.plugin.DaoInterceptor;
import io.github.ramerf.wind.core.plugin.ServiceInterceptor;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.spring.transaction.SpringManagedTransactionFactory;
import java.util.ArrayList;
import java.util.List;
import javax.sql.DataSource;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnSingleCandidate;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

/**
 * @author ramer
 * @since 2022.06.03
 */
@org.springframework.context.annotation.Configuration
@ConditionalOnSingleCandidate(DataSource.class)
@EnableConfigurationProperties(WindProperty.class)
@AutoConfigureAfter({DataSourceAutoConfiguration.class})
public class WindAutoConfiguration {
  @Autowired private WindProperty windProperty;

  @Autowired(required = false)
  private List<ITypeHandler<?, ?>> typeHandlers = new ArrayList<>();

  @Autowired(required = false)
  private List<DaoInterceptor> daoInterceptors = new ArrayList<>();

  @Autowired(required = false)
  private List<ServiceInterceptor> serviceInterceptors = new ArrayList<>();

  @Bean
  @ConditionalOnMissingBean
  public TransactionFactory transactionFactory() {
    return new SpringManagedTransactionFactory();
  }

  @Bean
  @ConditionalOnMissingBean
  public DaoFactory daoFactory(
      DataSource dataSource,
      TransactionFactory transactionFactory,
      ObjectProvider<IdGenerator> idGenerator) {
    final Configuration configuration = windProperty.getConfiguration();
    configuration.setJdbcEnvironment(new JdbcEnvironment(transactionFactory, dataSource));
    idGenerator.ifAvailable(configuration::setIdGenerator);
    daoInterceptors.forEach(configuration::addInterceptor);
    serviceInterceptors.forEach(configuration::addInterceptor);
    typeHandlers.forEach(configuration::addTypeHandler);
    return WindApplication.run(configuration).getDaoFactory();
  }

  @Bean
  @ConditionalOnMissingBean
  public Dao dao(DaoFactory daoFactory) {
    return daoFactory.getDao();
  }
}
