package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.annotation.Ds;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSourceHolder;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import io.github.ramerf.wind.core.service.BaseService;
import java.io.Serializable;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.*;

public class DynamicDataSourceAdaptor {

  public static <T, ID extends Serializable> BaseService<T, ID> intercept(
      BaseService<T, ID> baseService) {
    Enhancer enhancer = new Enhancer();
    enhancer.setSuperclass(baseService.getClass());
    enhancer.setCallback(new DynamicDataSourceInterceptor(baseService));
    //noinspection unchecked
    return (BaseService<T, ID>)
        enhancer.create(new Class[] {Dao.class}, new Object[] {baseService.getDao()});
  }

  @Slf4j
  public static class DynamicDataSourceInterceptor implements MethodInterceptor {
    private final BaseService<?, ?> target;

    public DynamicDataSourceInterceptor(BaseService<?, ?> target) {
      this.target = target;
    }

    @Override
    public Object intercept(
        final Object proxy, final Method method, final Object[] args, final MethodProxy methodProxy)
        throws Throwable {
      final Ds ds = method.getAnnotation(Ds.class);
      if (ds == null) {
        return method.invoke(target, args);
      }
      final Dao dao = target.getDao();
      try {
        DynamicDataSourceHolder.push(ds.value());
        return method.invoke(target, args);
      } catch (Exception e) {
        throw ExceptionUtil.unwrapThrowable(e);
      } finally {
        DynamicDataSourceHolder.poll();
      }
    }
  }
}
