package io.github.ramerf.wind.core.service;

import java.io.Serializable;

/**
 * 执行通用业务.
 *
 * @since 2020.10.28
 * @author ramer
 */
public class GenericLambdaService<T, ID extends Serializable> implements BaseService<T, ID> {
  private Class<T> clazz;

  public static <T, ID extends Serializable> GenericLambdaService<T, ID> with(
      Class<T> clazz, Class<ID> id) {
    final GenericLambdaService<T, ID> service = new GenericLambdaService<>();
    service.clazz = clazz;
    return service;
  }

  @Override
  public Class<T> getPoJoClass() {
    return clazz;
  }
}
