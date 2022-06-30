package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.service.BaseService;

/**
 * service公共方法拦截器.
 *
 * @since 2022.06.19
 * @author ramer
 * @see ServiceInterceptorChain
 * @see BaseService
 */
public interface ServiceInterceptor {

  /** 是否支持拦截当前类. */
  boolean supports(Class<?> clazz);

  Object intercept(Invocation invocation) throws Throwable;
}
