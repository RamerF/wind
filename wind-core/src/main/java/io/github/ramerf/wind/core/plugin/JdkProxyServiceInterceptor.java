package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JdkProxyServiceInterceptor implements InvocationHandler {

  private final Object target;
  private final ServiceInterceptorChain interceptorChain;

  public JdkProxyServiceInterceptor(Object target, ServiceInterceptorChain interceptorChain) {
    this.target = target;
    this.interceptorChain = interceptorChain;
  }

  @Override
  public Object invoke(final Object proxy, final Method method, final Object[] args)
      throws Throwable {
    try {
      final String name = method.getName();
      if (Plugins.QUERY_METHODS_SERVICE.contains(name)
          || Plugins.UPDATE_METHODS_SERVICE.contains(name)) {
        return interceptorChain.proceed(new Invocation(target, method, args, interceptorChain));
      }
      return method.invoke(target, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }
}
