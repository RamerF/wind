package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.service.BaseService;

/**
 * TODO WARN 用于拦截 {@link BaseService},
 *
 * <p>WindApplication中有getService(class)方法,在方法里面代理接入spring的时候需要扫描所有BaseService的子类先调用getService(class)代理一遍
 */
public interface ServiceInterceptor {

  /** 是否支持拦截当前类. */
  boolean supports(Class<?> clazz);

  Object intercept(Invocation invocation) throws Throwable;
}
