package io.github.ramerf.wind.core.ioc;

import io.github.ramerf.wind.core.exception.SimpleException;

/**
 * @author ramer
 * @since 23/12/2021
 */
public class SingletonBeanFactory implements BeanFactory {
  @Override
  public <T> T getBean(Class<T> clazz) {
    throw new SimpleException("Not Implement.");
  }
}
