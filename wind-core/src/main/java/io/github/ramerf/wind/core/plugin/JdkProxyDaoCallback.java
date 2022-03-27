package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class JdkProxyDaoCallback implements InvocationHandler {

  private final Object target;
  private final DaoInterceptor daoInterceptor;

  public JdkProxyDaoCallback(Object target, DaoInterceptor daoInterceptor) {
    this.target = target;
    this.daoInterceptor = daoInterceptor;
  }

  @Override
  public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
    try {
      final String name = method.getName();
      if (Plugins.QUERY_METHODS_DAO.contains(name) || Plugins.UPDATE_METHODS_DAO.contains(name)) {
        return daoInterceptor.intercept(new Invocation(target, method, args));
      }
      return method.invoke(target, method, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }
}
