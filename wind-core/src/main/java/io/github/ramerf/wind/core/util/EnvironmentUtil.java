package io.github.ramerf.wind.core.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

/**
 * @author Tang Xiaofeng
 * @since 2019/11/21
 */
@Slf4j
@Component
public class EnvironmentUtil implements ApplicationContextAware {
  private static Environment environment = null;

  public boolean is(Env env) {
    return env.toString().equalsIgnoreCase(environment.getProperty("spring.profiles.active"));
  }

  @Override
  public void setApplicationContext(final ApplicationContext applicationContext)
      throws BeansException {
    environment = applicationContext.getBean(Environment.class);
  }

  public enum Env {
    /** 默认环境. */
    DEFAULT,
    TEST,
    DEV,
    PROD
  }
}
