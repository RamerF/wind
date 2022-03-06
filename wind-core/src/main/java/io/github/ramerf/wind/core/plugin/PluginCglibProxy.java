package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.InvocationHandler;

@Slf4j
public class PluginCglibProxy implements InvocationHandler {

  private final Object target;
  private final Interceptor interceptor;

  public PluginCglibProxy(Object target, Interceptor interceptor) {
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
      return method.invoke(target, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }

  //
  // @Override
  // public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy)
  //     throws Throwable {
  //   // 注意这里的方法调用，不是用反射哦！！！
  //   log.info("cglib intercept:[{}]", obj.getClass());
  //   log.info("cglib intercept:[{}]", method);
  //   log.info("cglib intercept:[{}]", args);
  //   log.info("cglib intercept:[{}]", proxy);
  //   Object object = proxy.invokeSuper(obj, args);
  //
  //   try {
  //     return interceptor.intercept(new Invocation(target, method, args));
  //   } catch (Exception e) {
  //     throw ExceptionUtil.unwrapThrowable(e);
  //   }
  //   return object;
  // }
}
