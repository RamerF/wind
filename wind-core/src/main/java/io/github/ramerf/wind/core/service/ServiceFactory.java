package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.annotation.Ds;
import io.github.ramerf.wind.core.annotation.Transactional;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSourceHolder;
import io.github.ramerf.wind.core.plugin.ProxyChain;
import io.github.ramerf.wind.core.plugin.ProxyChain.ProxyChainPoint;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.io.Serializable;
import java.util.ArrayDeque;
import java.util.Deque;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodInterceptor;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
public class ServiceFactory {
  @SuppressWarnings("unchecked")
  public static <S extends BaseService<T, ID>, T, ID extends Serializable> S getService(
      @Nonnull S service) {
    Deque<ProxyChainPoint> points = new ArrayDeque<>();
    // dynamic data source
    points.add(
        (proxyObj, chain, method, args, methodProxy) -> {
          Object result = null;
          final Ds ds = method.getAnnotation(Ds.class);
          if (ds == null) {
            return chain.proceed(proxyObj, method, args, methodProxy);
          }
          try {
            DynamicDataSourceHolder.push(ds.value());
            return chain.proceed(proxyObj, method, args, methodProxy);
          } finally {
            //
          }
        });
    // tranactional
    points.add(
        (proxyObj, chain, method, args, methodProxy) -> {
          final Transactional transactional = method.getAnnotation(Transactional.class);
          if (transactional == null) {
            return chain.proceed(proxyObj, method, args, methodProxy);
          }
          final BaseService<?, ?> target = (BaseService<?, ?>) chain.getTarget();
          final Dao dao = target.getDao();
          try {
            final Object result = chain.proceed(proxyObj, method, args, methodProxy);
            dao.commit(true);
            return result;
          } catch (Exception e) {
            final Class<? extends Exception> rollbackFor = transactional.rollbackFor();
            if (rollbackFor.isAssignableFrom(e.getClass())) {
              dao.rollback(true);
            } else {
              dao.commit(true);
            }
            throw new DataAccessException(ExceptionUtil.unwrapThrowable(e));
          } finally {
            // TODO WARN close connection
          }
        });
    Enhancer enhancer = new Enhancer();
    enhancer.setSuperclass(service.getClass());
    enhancer.setCallback(
        (MethodInterceptor)
            (proxyObj, method, args, methodProxy) -> {
              log.info(
                  "proxy method======================[{},{}]======================",
                  method.getName(),
                  method.getDeclaringClass());
              final Object proceed =
                  new ProxyChain(service, new ArrayDeque<>(points))
                      .proceed(proxyObj, method, args, methodProxy);
              if (method.isAnnotationPresent(Ds.class)) {
                DynamicDataSourceHolder.poll();
              }
              return proceed;
            });
    return (S) enhancer.create(new Class[] {Dao.class}, new Object[] {service.getDao()});
  }
}
