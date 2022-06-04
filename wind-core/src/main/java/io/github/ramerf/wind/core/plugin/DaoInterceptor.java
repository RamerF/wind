package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.executor.Dao;
import java.util.Properties;

/** 用于拦截 {@link Dao} */
public interface DaoInterceptor {

  Object intercept(Invocation invocation) throws Throwable;

  default void setProperties(Properties properties) {
    // NOP
  }
}
