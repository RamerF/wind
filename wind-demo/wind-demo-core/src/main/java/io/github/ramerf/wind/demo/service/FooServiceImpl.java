package io.github.ramerf.wind.demo.service;

import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.demo.entity.Foo;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Foo service.
 *
 * @since 2022.06.19
 * @author ramer
 */
@Slf4j
public class FooServiceImpl extends BaseServiceImpl<Foo, Long> implements FooService {
  public FooServiceImpl(final Dao dao) {
    super(dao);
  }
}
