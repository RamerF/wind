package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;

/** dao拦截器 */
@Slf4j
public class CglibDaoInterceptor implements MethodInterceptor {

  private final Object target;
  private final DaoInterceptorChain interceptorChain;

  public CglibDaoInterceptor(Object target, DaoInterceptorChain interceptorChain) {
    this.target = target;
    this.interceptorChain = interceptorChain;
  }

  @Override
  public Object intercept(
      final Object proxy, final Method method, final Object[] args, final MethodProxy methodProxy)
      throws Throwable {
    try {
      final String name = method.getName();
      if (Plugins.QUERY_METHODS_DAO.contains(name) || Plugins.UPDATE_METHODS_DAO.contains(name)) {
        return interceptorChain.proceed(new Invocation(target, method, args, interceptorChain));
      }
      return method.invoke(target, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }
}
