package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;

/**
 * 执行通用业务.
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
public class GenericService<T extends AbstractEntityPoJo> implements BaseService<T> {
  private Class<T> clazz;

  public static <T extends AbstractEntityPoJo> GenericService<T> with(Class<T> clazz) {
    final GenericService<T> service = new GenericService<>();
    service.clazz = clazz;
    return service;
  }

  @Override
  public Class<T> getPoJoClass() {
    return clazz;
  }
}
