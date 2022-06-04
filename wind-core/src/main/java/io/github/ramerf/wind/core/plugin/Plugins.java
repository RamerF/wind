package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.Executor;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.core.service.GenericService;
import java.util.*;
import net.sf.cglib.proxy.Enhancer;

public class Plugins {
  public static final Set<String> UPDATE_METHODS_DAO;
  public static final Set<String> QUERY_METHODS_DAO;

  public static final Set<String> UPDATE_METHODS_SERVICE;
  public static final Set<String> QUERY_METHODS_SERVICE;

  static {
    Set<String> updateMethods = new HashSet<>();
    updateMethods.add("create");
    updateMethods.add("createBatch");
    updateMethods.add("update");
    updateMethods.add("updateBatch");
    updateMethods.add("delete");
    UPDATE_METHODS_DAO = Collections.unmodifiableSet(updateMethods);

    Set<String> queryMethods = new HashSet<>();
    queryMethods.add("fetchOne");
    queryMethods.add("fetchAll");
    queryMethods.add("fetchPage");
    queryMethods.add("fetchCount");
    queryMethods.add("fetchOneBySql");
    queryMethods.add("fetchListBySql");
    queryMethods.add("countBySql");
    QUERY_METHODS_DAO = Collections.unmodifiableSet(queryMethods);
  }

  static {
    Set<String> updateMethods = new HashSet<>();
    updateMethods.add("create");
    updateMethods.add("createBatch");
    updateMethods.add("update");
    updateMethods.add("updateBatch");
    updateMethods.add("delete");
    UPDATE_METHODS_SERVICE = Collections.unmodifiableSet(updateMethods);

    Set<String> queryMethods = new HashSet<>();
    queryMethods.add("count");
    queryMethods.add("getOne");
    queryMethods.add("list");
    queryMethods.add("page");
    queryMethods.add("populateMapping");
    QUERY_METHODS_SERVICE = Collections.unmodifiableSet(queryMethods);
  }

  public static Object wrap(Object target, DaoInterceptorChain chain, final Object[] args) {
    Class<?> clazz = target.getClass();
    /* jdk代理
    Class<?>[] interfaces = BeanUtils.getAllInterfaces(clazz);
    if (interfaces.length > 0) {
      return Proxy.newProxyInstance(
          clazz.getClassLoader(), interfaces, new PluginJdkProxyCallback(target, interceptor));
    }*/
    if (Dao.class.isAssignableFrom(clazz)) {
      Enhancer enhancer = new Enhancer();
      enhancer.setSuperclass(clazz);
      enhancer.setCallback(new CglibDaoInterceptor(target, chain));
      target =
          enhancer.create(new Class[] {Configuration.class, Executor.class, boolean.class}, args);
    }
    return target;
  }


  public static Object wrap(Object target, ServiceInterceptorChain chain, final Object[] args) {
    final Class<?> clazz = target.getClass();
    Enhancer enhancer = new Enhancer();
    enhancer.setSuperclass(clazz);
    enhancer.setCallback(new CglibServiceInterceptor(target, chain));
    if (BaseServiceImpl.class.isAssignableFrom(clazz)) {
      target = enhancer.create(new Class[] {Dao.class}, args);
    } //
    else if (GenericService.class.isAssignableFrom(clazz)) {
      target = enhancer.create(new Class[] {Dao.class, Class.class, Class.class}, args);
    }
    return target;
  }
}
