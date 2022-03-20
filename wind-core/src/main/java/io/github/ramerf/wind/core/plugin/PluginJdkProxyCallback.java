package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class PluginJdkProxyCallback implements InvocationHandler {

  private final Object target;
  private final Interceptor interceptor;

  public PluginJdkProxyCallback(Object target, Interceptor interceptor) {
    this.target = target;
    this.interceptor = interceptor;
  }

  @Override
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    try {
      final String name = method.getName();
      if (Plugins.QUERY_METHODS.contains(name) || Plugins.UPDATE_METHODS.contains(name)) {
        return interceptor.intercept(new Invocation(target, method, args));
      }
      return method.invoke(target, method, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }
}
