package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import java.util.Properties;

/** 用于拦截 {@link Query}和{@link Update} */
public interface Interceptor {

  /** 是否支持拦截当前类. */
  boolean supports(Class<?> clazz);

  Object intercept(Invocation invocation) throws Throwable;

  default Object plugin(Object target, final Object[] args) {
    return Plugins.wrap(target, this, args);
  }

  default void setProperties(Properties properties) {
    // NOP
  }
}
