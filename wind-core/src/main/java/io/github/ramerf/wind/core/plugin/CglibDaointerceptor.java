package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;

@Slf4j
public class CglibDaointerceptor implements MethodInterceptor {

  private final Object target;
  private final DaoInterceptor interceptor;

  public CglibDaointerceptor(Object target, DaoInterceptor interceptor) {
    this.target = target;
    this.interceptor = interceptor;
  }

  @Override
  public Object intercept(
      final Object proxy, final Method method, final Object[] args, final MethodProxy methodProxy)
      throws Throwable {
    try {
      final String name = method.getName();
      if (Plugins.QUERY_METHODS_DAO.contains(name) || Plugins.UPDATE_METHODS_DAO.contains(name)) {
        return interceptor.intercept(new Invocation(target, method, args));
      }
      return method.invoke(target, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }
}
