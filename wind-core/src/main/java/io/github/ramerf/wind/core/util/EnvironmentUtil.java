package io.github.ramerf.wind.core.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.env.Environment;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

/**
 * @author Tang Xiaofeng
 * @since 2019/11/21
 */
@Slf4j
@Component
public class EnvironmentUtil implements ApplicationContextAware {
  private static Environment environment = null;

  /**
   * Return the property value associated with the given key, or {@code null} if the key cannot be
   * resolved.
   *
   * @param key the property name to resolve
   * @see #getProperty(String, String)
   * @see #getProperty(String, Class)
   */
  @Nullable
  public static String getProperty(String key) {
    return environment.getProperty(key);
  }

  /**
   * Return the property value associated with the given key, or {@code defaultValue} if the key
   * cannot be resolved.
   *
   * @param key the property name to resolve
   * @param defaultValue the default value to return if no value is found
   * @see #getProperty(String, Class)
   */
  public static String getProperty(String key, String defaultValue) {
    return environment.getProperty(key, defaultValue);
  }

  /**
   * Return the property value associated with the given key, or {@code null} if the key cannot be
   * resolved.
   *
   * @param key the property name to resolve
   * @param targetType the expected type of the property value
   */
  @Nullable
  public static <T> T getProperty(String key, Class<T> targetType) {
    return environment.getProperty(key, targetType);
  }

  /**
   * Return the property value associated with the given key, or {@code defaultValue} if the key
   * cannot be resolved.
   *
   * @param key the property name to resolve
   * @param targetType the expected type of the property value
   * @param defaultValue the default value to return if no value is found
   */
  public static <T> T getProperty(String key, Class<T> targetType, T defaultValue) {
    return environment.getProperty(key, targetType, defaultValue);
  }

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
