package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.executor.Query;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

/**
 * 该类用于在接口默认方法中获取bean.由于违背Ioc,不建议使用{@link #getBean(String)}获取bean. 使用方式参考: {@link
 * Query#getInstance()}
 *
 * @author Tang Xiaofeng
 * @since 2020/1/14
 */
@Slf4j
@Component
public class AppContextInject implements ApplicationContextAware {
  public static ApplicationContext context;

  public static <T> T getBean(final Class<T> requiredType) {
    return context.getBean(requiredType);
  }

  @SuppressWarnings("unchecked")
  public static <T> T getBean(final String beanName) {
    return (T) context.getBean(beanName);
  }

  @Override
  public void setApplicationContext(@NonNull ApplicationContext applicationContext)
      throws BeansException {
    AppContextInject.context = applicationContext;
    log.info("setApplicationContext:[Inject ApplicationContext]");
  }
}
