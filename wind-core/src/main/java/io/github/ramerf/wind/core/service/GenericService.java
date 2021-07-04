package io.github.ramerf.wind.core.service;

import java.io.Serializable;

/**
 * 执行通用业务.
 *
 * @since 2020.10.28
 * @author ramer
 */
public class GenericService<T, ID extends Serializable> implements BaseService<T, ID> {
  private Class<T> clazz;

  public static <T, ID extends Serializable> GenericService<T, ID> with(
      Class<T> clazz, Class<ID> id) {
    final GenericService<T, ID> service = new GenericService<>();
    service.clazz = clazz;
    return service;
  }

  @Override
  public Class<T> getPoJoClass() {
    return clazz;
  }
}
