package io.github.ramerf.wind.core.plugin;

import java.util.*;

public class ServiceInterceptorChain {

  private final List<ServiceInterceptor> serviceInterceptors = new ArrayList<>();

  public Object pluginAll(Object target, final Class<?> clazz, final Object[] args) {
    for (ServiceInterceptor serviceInterceptor : serviceInterceptors) {
      if (serviceInterceptor.supports(clazz)) {
        target = serviceInterceptor.plugin(target, args);
      }
    }
    return target;
  }

  public void addInterceptor(ServiceInterceptor serviceInterceptor) {
    serviceInterceptors.add(serviceInterceptor);
  }

  public List<ServiceInterceptor> getInterceptors() {
    return Collections.unmodifiableList(serviceInterceptors);
  }
}
