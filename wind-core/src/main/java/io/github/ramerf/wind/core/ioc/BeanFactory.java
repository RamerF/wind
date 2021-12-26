package io.github.ramerf.wind.core.ioc;

/**
 * @author ramer
 * @since 23/12/2021
 */
public interface BeanFactory {

  <T> T getBean(Class<T> clazz);
}
