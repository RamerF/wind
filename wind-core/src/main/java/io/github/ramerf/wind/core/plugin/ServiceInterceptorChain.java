package io.github.ramerf.wind.core.plugin;

import java.util.*;

/**
 * The type Service interceptor chain.
 *
 * @since 2022.06.19
 * @author ramer
 * @see ServiceInterceptor
 */
public class ServiceInterceptorChain implements InterceptorChain {

  private final List<ServiceInterceptor> serviceInterceptors = new ArrayList<>();
  private int index = -1;

  /** 代理加入拦截器 */
  public Object pluginAll(Object target, final Class<?> clazz, final Object[] args) {
    final ServiceInterceptorChain chain = new ServiceInterceptorChain();
    for (ServiceInterceptor serviceInterceptor : serviceInterceptors) {
      if (serviceInterceptor.supports(clazz)) {
        chain.addInterceptor(serviceInterceptor);
      }
    }
    return Plugins.wrap(target, chain, args);
  }

  /** 获取当前service支持的拦截器 */
  public ServiceInterceptorChain support(final Class<?> clazz) {
    final ServiceInterceptorChain chain = new ServiceInterceptorChain();
    for (ServiceInterceptor serviceInterceptor : serviceInterceptors) {
      if (serviceInterceptor.supports(clazz)) {
        chain.addInterceptor(serviceInterceptor);
      }
    }
    return chain;
  }

  public void addInterceptor(ServiceInterceptor serviceInterceptor) {
    serviceInterceptors.add(serviceInterceptor);
  }

  public List<ServiceInterceptor> getInterceptors() {
    return Collections.unmodifiableList(serviceInterceptors);
  }

  @Override
  public Object proceed(final Invocation invocation) throws Throwable {
    Object obj;
    final List<ServiceInterceptor> interceptors = getInterceptors();
    if (++index == interceptors.size()) {
      obj = invocation.invoke();
    } else {
      obj = interceptors.get(index).intercept(invocation);
    }
    return obj;
  }

  @Override
  public void reset() {
    index = -1;
  }
}
