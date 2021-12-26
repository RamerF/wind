package io.github.ramerf.wind.core.ioc;

/**
 * @author ramer
 * @since 23/12/2021
 */
public class SingletonBeanDefinition extends AbstractBeanDefinition {
  public SingletonBeanDefinition() {
    setSingleton(true);
  }
}
