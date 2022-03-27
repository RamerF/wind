package io.github.ramerf.wind.core.plugin;

import java.util.*;

public class DaoInterceptorChain {

  private final List<DaoInterceptor> daoInterceptors = new ArrayList<>();

  public Object pluginAll(Object target, final Object[] args) {
    for (DaoInterceptor daoInterceptor : daoInterceptors) {
      target = daoInterceptor.plugin(target, args);
    }
    return target;
  }

  public void addInterceptor(DaoInterceptor daoInterceptor) {
    daoInterceptors.add(daoInterceptor);
  }

  public List<DaoInterceptor> getInterceptors() {
    return Collections.unmodifiableList(daoInterceptors);
  }
}
