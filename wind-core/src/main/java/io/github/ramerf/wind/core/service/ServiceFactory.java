package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.executor.Dao;
import java.io.Serializable;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
public class ServiceFactory {
  public static <S extends I, I extends BaseService<T, ID>, T, ID extends Serializable>
      I getService(S service) {
    final Dao dao = service.getDao();
    final Class<T> clazz = service.getPoJoClass();
    final Class<ID> id = service.getIdClass();
    //noinspection unchecked
    return (I)
        dao.getConfiguration()
            .getServiceInterceptorChain()
            .support(clazz)
            .pluginAll(service, clazz, new Object[] {dao, clazz, id});
  }

  public static <T, ID extends Serializable> BaseService<T, ID> getService(
      final Dao dao, final Class<T> clazz, final Class<ID> id) {
    final GenericService<T, ID> service = new GenericService<>(dao, clazz, id);
    //noinspection unchecked
    return (BaseService<T, ID>)
        dao.getConfiguration()
            .getServiceInterceptorChain()
            .support(clazz)
            .pluginAll(service, clazz, new Object[] {dao, clazz, id});
  }
}
