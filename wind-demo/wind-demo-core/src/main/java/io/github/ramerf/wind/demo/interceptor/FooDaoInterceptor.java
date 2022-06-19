package io.github.ramerf.wind.demo.interceptor;

import io.github.ramerf.wind.core.plugin.DaoInterceptor;
import io.github.ramerf.wind.core.plugin.Invocation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class FooDaoInterceptor implements DaoInterceptor {

  @Override
  public Object intercept(final Invocation invocation) throws Throwable {
    log.info("FooDaoInterceptor:[{}]", "---------Foo Dao Interceptor---------");
    log.info("FooDaoInterceptor:[{}]", invocation.getTarget());
    log.info("FooDaoInterceptor:[{}]", invocation.getMethod());
    log.info("FooDaoInterceptor:[{}]", invocation.isWriteMethod());
    final Object[] args = invocation.getArgs();
    for (final Object arg : args) {
      log.info("FooDaoInterceptor:args[{}]", arg);
    }
    return invocation.proceed();
  }
}
