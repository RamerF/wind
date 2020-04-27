package io.github.ramerf.mybatisturbo.core.service;

import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.entity.response.AbstractEntityResponse;
import io.github.ramerf.mybatisturbo.core.repository.BaseRepository;

/**
 * 通用业务方法.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/20
 */
@SuppressWarnings({"unused", "unchecked"})
public class BaseServiceImpl<
        T extends AbstractEntityPoJo,
        E extends AbstractEntityResponse,
        R extends BaseRepository<T, Long>>
    implements BaseService<T, E> {
  private final R repository;

  public BaseServiceImpl(R repository) {
    this.repository = repository;
  }

  @Override
  public <U extends BaseRepository<T, Long>> U getRepository() throws RuntimeException {
    return (U) repository;
  }
}
