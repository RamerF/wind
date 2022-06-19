package io.github.ramerf.wind.core.plugin;

/**
 * The interface Service interceptor.
 *
 * @since 2022.06.19
 * @author ramer
 * @see ServiceInterceptorChain
 */
public interface ServiceInterceptor {

  /** 是否支持拦截当前类. */
  boolean supports(Class<?> clazz);

  Object intercept(Invocation invocation) throws Throwable;
}
