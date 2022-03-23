package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.service.BaseService;

/** TODO WARN 用于拦截 {@link BaseService} */
public interface ServiceInterceptor {

  /** 是否支持拦截当前类. */
  boolean supports(Class<?> clazz);

  Object intercept(Invocation invocation) throws Throwable;

  // default Object plugin(Object target, final Object[] args) {
  //   return Plugins.wrap(target, this, args);
  // }
}
