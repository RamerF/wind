package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.annotation.Transactional;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import io.github.ramerf.wind.core.service.BaseService;
import java.io.Serializable;
import java.lang.reflect.Method;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.proxy.*;

public class TransactionalAdaptor {

  public static <T, ID extends Serializable> BaseService<T, ID> intercept(
      BaseService<T, ID> baseService) {
    Enhancer enhancer = new Enhancer();
    enhancer.setSuperclass(baseService.getClass());
    enhancer.setCallback(new TransactionalInterceptor(baseService));
    //noinspection unchecked
    return (BaseService<T, ID>)
        enhancer.create(new Class[] {Dao.class}, new Object[] {baseService.getDao()});
  }

  @Slf4j
  private static class TransactionalInterceptor implements MethodInterceptor {
    private final BaseService<?, ?> target;

    public TransactionalInterceptor(BaseService<?, ?> target) {
      this.target = target;
    }

    @Override
    public Object intercept(
        final Object proxy, final Method method, final Object[] args, final MethodProxy methodProxy)
        throws Throwable {
      final Transactional transactional = method.getAnnotation(Transactional.class);
      if (transactional == null) {
        return method.invoke(target, args);
      }
      final Dao dao = target.getDao();
      try {
        final Object result = method.invoke(target, args);
        dao.commit(true);
        return result;
      } catch (Exception e) {
        final Class<? extends Exception> rollbackFor = transactional.rollbackFor();
        if (rollbackFor.isAssignableFrom(e.getClass())) {
          dao.rollback(true);
        } else {
          dao.commit(true);
        }
        throw ExceptionUtil.unwrapThrowable(e);
      } finally {
        //
      }
    }
  }
}
