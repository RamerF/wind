package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.AbstractEntityResponse;

/**
 * 通用业务方法.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/20
 */
@SuppressWarnings({"unused", "unchecked"})
public class BaseServiceImpl<T extends AbstractEntityPoJo, E extends AbstractEntityResponse, R>
    implements BaseService<T> {
  private final R repository;

  public BaseServiceImpl(R repository) {
    this.repository = repository;
  }

  @Override
  public <U> U getRepository() throws RuntimeException {
    return (U) repository;
  }
}
