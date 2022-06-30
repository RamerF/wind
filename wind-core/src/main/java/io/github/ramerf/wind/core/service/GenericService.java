package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.executor.Dao;
import java.io.Serializable;

/**
 * 通用service.
 *
 * @since 2020.10.28
 * @author ramer
 * @see ServiceFactory
 */
final class GenericService<T, ID extends Serializable> implements BaseService<T, ID> {
  private final Dao dao;
  private final Class<T> clazz;
  private final Class<ID> idClazz;

  /** 生成对应clazz的service */
  public GenericService(final Dao dao, Class<T> clazz, Class<ID> idClazz) {
    this.dao = dao;
    this.clazz = clazz;
    this.idClazz = idClazz;
  }

  public static <T, ID extends Serializable> BaseService<T, ID> with(
      final Dao dao, final Class<T> clazz, final Class<ID> id) {
    final GenericService<T, ID> service = new GenericService<>(dao, clazz, id);
    //noinspection unchecked
    return (BaseService<T, ID>)
        dao.getConfiguration()
            .getServiceInterceptorChain()
            .support(clazz)
            .pluginAll(service, clazz, new Object[] {dao, clazz, id});
  }

  @Override
  public Dao getDao() {
    return dao;
  }

  @Override
  public Class<T> getPoJoClass() {
    return clazz;
  }

  @Override
  public Class<ID> getIdClass() {
    return idClazz;
  }
}
