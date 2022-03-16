package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.Configuration;

/**
 * @author ramer
 * @since 2022.03.12
 */
// TODO WARN 聚合query/update方法,去掉泛型
public class DaoImpl implements Dao {
  private Configuration configuration;
  private Query query;
  private Update update;

  @Override
  public Configuration getConfiguration() {
    return configuration;
  }
}
