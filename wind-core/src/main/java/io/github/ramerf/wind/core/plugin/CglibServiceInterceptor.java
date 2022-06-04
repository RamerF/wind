package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;

@Slf4j
public class CglibServiceInterceptor implements MethodInterceptor {

  private final Object target;
  private final ServiceInterceptorChain interceptorChain;

  public CglibServiceInterceptor(Object target, ServiceInterceptorChain interceptorChain) {
    this.target = target;
    this.interceptorChain = interceptorChain;
  }

  @Override
  public Object intercept(
      final Object proxy, final Method method, final Object[] args, final MethodProxy methodProxy)
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
