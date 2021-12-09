package io.github.ramerf.wind.core.service;

import java.io.Serializable;

/**
 * 通用service.
 *
 * @since 2020.10.28
 * @author ramer
 */
public class GenericService<T, ID extends Serializable> implements BaseService<T, ID> {
  private Class<T> clazz;

  /**
   * 生成对应clazz的service
   *
   * @param clazz 操作的实体
   * @param id 主键
   */
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
