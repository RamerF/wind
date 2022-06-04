package io.github.ramerf.wind.core.plugin;

import java.util.*;

public class ServiceInterceptorChain implements InterceptorChain {

  private final List<ServiceInterceptor> serviceInterceptors = new ArrayList<>();
  private int index = -1;

  public Object pluginAll(Object target, final Class<?> clazz, final Object[] args) {
    final ServiceInterceptorChain chain = new ServiceInterceptorChain();
    for (ServiceInterceptor serviceInterceptor : serviceInterceptors) {
      if (serviceInterceptor.supports(clazz)) {
        chain.addInterceptor(serviceInterceptor);
      }
    }
    return Plugins.wrap(target, chain, args);
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
