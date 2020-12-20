package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.io.Serializable;

/**
 * 执行通用业务.
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
public class GenericService<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
    implements BaseService<T, ID> {
  private Class<T> clazz;

  public static <T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
      GenericService<T, ID> with(Class<T> clazz, Class<ID> id) {
    final GenericService<T, ID> service = new GenericService<>();
    service.clazz = clazz;
    return service;
  }

  @Override
  public Class<T> getPoJoClass() {
    return clazz;
  }
}
