package io.github.ramerf.wind.core.ioc;

import io.github.ramerf.wind.core.exception.WindException;

/**
 * @author ramer
 * @since 23/12/2021
 */
public class PrototypeBeanDefinition extends AbstractBeanDefinition {
  @Override
  public <T> T get() {
    throw new WindException("Not Implement.");
  }
}
