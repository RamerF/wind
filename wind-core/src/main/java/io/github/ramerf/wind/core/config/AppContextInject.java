package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.ioc.ApplicationContext;
import lombok.extern.slf4j.Slf4j;

/**
 * 该类用于在接口默认方法中获取bean.由于违背Ioc,不建议使用{@link #getBean(Class)}}获取bean.
 *
 * <p>使用方式参考: {@link Query#getInstance(Class)}
 *
 * @author ramer
 * @since 2020/1/14
 */
@Slf4j
public class AppContextInject {
  private static ApplicationContext context;

  public static void initial(final ApplicationContext context) {
    AppContextInject.context = context;
  }

  public static <T> T getBean(final Class<T> requiredType) {
    return context.getBean(requiredType);
  }

  @SuppressWarnings("unchecked")
  public static <T> T getBean(final String beanName) {
    return (T) context.getBean(beanName);
  }
}
