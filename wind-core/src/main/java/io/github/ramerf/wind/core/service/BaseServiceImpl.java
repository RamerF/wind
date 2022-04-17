package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.executor.Dao;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * 预留实现.
 *
 * @author ramer
 * @since 2019/12/20
 */
public abstract class BaseServiceImpl<T, ID extends Serializable> implements BaseService<T, ID> {
  protected final Dao dao;

  public BaseServiceImpl(final Dao dao) {
    this.dao = dao;
  }

  @Override
  public Dao getDao() {
    return dao;
  }

  @Override
  public Class<T> getPoJoClass() {
    return getTypeParameter(getClass());
  }

  private <U> Class<U> getTypeParameter(Class<?> clazz) {
    Type genericSuperclass = clazz.getGenericSuperclass();
    if (genericSuperclass instanceof Class) {
      if (BaseServiceImpl.class != genericSuperclass) {
        return getTypeParameter(clazz.getSuperclass());
      }
      throw new WindException(
          getClass() + " extends BaseServiceImpl, but missing the type parameter");
    }
    Type rawType = ((ParameterizedType) genericSuperclass).getActualTypeArguments()[0];
    //noinspection unchecked
    return (Class<U>) rawType;
  }
}
