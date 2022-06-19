package io.github.ramerf.wind.core.plugin;

/**
 * @author ramer
 * @since 2022.06.04
 * @see DaoInterceptorChain
 * @see ServiceInterceptorChain
 */
public interface InterceptorChain {
  Object proceed(Invocation invocation) throws Throwable;

  void reset();
}
