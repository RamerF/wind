package io.github.ramerf.wind.spring.boot.annotation;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.plugin.ServiceInterceptor;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import java.util.Map;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

/**
 * service实例化后,进行代理加入拦截器.
 *
 * @since 2022.06.04
 * @author ramer
 * @see BaseServiceImpl
 */
@Slf4j
public class ServiceRegistry implements ApplicationContextAware, BeanPostProcessor {

  private ApplicationContext applicationContext;

  @Override
  public void setApplicationContext(@Nonnull final ApplicationContext applicationContext)
      throws BeansException {
    this.applicationContext = applicationContext;
  }

  @Override
  public Object postProcessAfterInitialization(
      @Nonnull final Object bean, @Nonnull final String beanName) throws BeansException {
    if (!isBaseService(bean)) {
      return BeanPostProcessor.super.postProcessAfterInitialization(bean, beanName);
    }
    final Map<String, ServiceInterceptor> serviceInterceptorMap =
        applicationContext.getBeansOfType(ServiceInterceptor.class);
    final Configuration configuration = applicationContext.getBean(Dao.class).getConfiguration();
    @SuppressWarnings("rawtypes")
    final BaseServiceImpl baseService = (BaseServiceImpl) bean;
    final Class<?> clazz = baseService.getPoJoClass();
    return configuration
        .getServiceInterceptorChain()
        .pluginAll(bean, clazz, new Object[] {baseService.getDao()});
  }

  private boolean isBaseService(final Object bean) {
    return bean instanceof BaseServiceImpl;
  }
}
