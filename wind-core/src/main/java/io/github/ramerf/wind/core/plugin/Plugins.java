package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.lang.reflect.Proxy;
import java.util.*;
import net.sf.cglib.proxy.Enhancer;

public class Plugins {
  public static final Set<String> UPDATE_METHODS;
  public static final Set<String> QUERY_METHODS;

  static {
    Set<String> updateMethods = new HashSet<>();
    updateMethods.add("create");
    updateMethods.add("createBatch");
    updateMethods.add("update");
    updateMethods.add("updateBatch");
    updateMethods.add("delete");
    UPDATE_METHODS = Collections.unmodifiableSet(updateMethods);

    Set<String> queryMethods = new HashSet<>();
    queryMethods.add("fetchOne");
    queryMethods.add("fetchAll");
    queryMethods.add("fetchPage");
    queryMethods.add("fetchCount");
    queryMethods.add("fetchOneBySql");
    queryMethods.add("fetchListBySql");
    queryMethods.add("countBySql");
    QUERY_METHODS = Collections.unmodifiableSet(queryMethods);
  }

  public static Object wrap(Object target, Interceptor interceptor, final Object[] args) {
    Class<?> clazz = target.getClass();
    Class<?>[] interfaces = BeanUtils.getAllInterfaces(clazz);
    if (false) {
      return Proxy.newProxyInstance(
          clazz.getClassLoader(), interfaces, new PluginJdkProxyCallback(target, interceptor));
    }
    //
    else if (Dao.class.isAssignableFrom(clazz)) {
      Enhancer enhancer = new Enhancer();
      // 继承被代理类
      enhancer.setSuperclass(target.getClass());
      // 设置回调
      enhancer.setCallback(new PluginCglibProxyCallback(target, interceptor));
      // TODO WARN 代理不同的参数
      if (Dao.class.isAssignableFrom(clazz)) {
        target =
            enhancer.create(new Class[] {Configuration.class, Executor.class, boolean.class}, args);
      }
    }
    return target;
  }
}
