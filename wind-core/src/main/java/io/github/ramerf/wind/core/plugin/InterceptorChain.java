package io.github.ramerf.wind.core.plugin;

import java.util.*;

public class InterceptorChain {

  private final List<Interceptor> interceptors = new ArrayList<>();

  // TODO WARN 这个clazz有待商榷
  public Object pluginAll(Object target, final Class<?> clazz, final Object[] args) {
    for (Interceptor interceptor : interceptors) {
      if (interceptor.supports(clazz)) {
        target = interceptor.plugin(target, args);
      }
    }
    return target;
  }

  public void addInterceptor(Interceptor interceptor) {
    interceptors.add(interceptor);
  }

  public List<Interceptor> getInterceptors() {
    return Collections.unmodifiableList(interceptors);
  }
}
