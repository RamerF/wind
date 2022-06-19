package io.github.ramerf.wind.demo.interceptor;

import io.github.ramerf.wind.core.plugin.Invocation;
import io.github.ramerf.wind.core.plugin.ServiceInterceptor;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class FooServiceInterceptor implements ServiceInterceptor {

  @Override
  public boolean supports(final Class<?> clazz) {
    return Foo.class.isAssignableFrom(clazz);
  }

  @Override
  public Object intercept(final Invocation invocation) throws Throwable {
    log.info("FooServiceInterceptor:[{}]", "---------Foo Service Interceptor---------");
    log.info("FooServiceInterceptor:[{}]", invocation.getTarget());
    log.info("FooServiceInterceptor:[{}]", invocation.getMethod());
    log.info("FooServiceInterceptor:[{}]", invocation.isWriteMethod());
    final Object[] args = invocation.getArgs();
    for (final Object arg : args) {
      log.info("FooServiceInterceptor:args[{}]", arg);
    }
    return invocation.proceed();
  }
}
