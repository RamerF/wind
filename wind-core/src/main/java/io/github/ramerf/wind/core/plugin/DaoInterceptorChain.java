package io.github.ramerf.wind.core.plugin;

import java.util.*;

public class DaoInterceptorChain implements InterceptorChain {

  private final List<DaoInterceptor> daoInterceptors = new ArrayList<>();
  private int index = -1;

  /** 代理加入拦截器 */
  public Object pluginAll(Object target, final Object[] args) {
    final DaoInterceptorChain chain = new DaoInterceptorChain();
    for (DaoInterceptor interceptor : daoInterceptors) {
      chain.addInterceptor(interceptor);
    }
    return Plugins.wrap(target, chain, args);
  }

  public void addInterceptor(DaoInterceptor daoInterceptor) {
    daoInterceptors.add(daoInterceptor);
  }

  public List<DaoInterceptor> getInterceptors() {
    return Collections.unmodifiableList(daoInterceptors);
  }

  @Override
  public Object proceed(final Invocation invocation) throws Throwable {
    Object obj;
    final List<DaoInterceptor> interceptors = getInterceptors();
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
