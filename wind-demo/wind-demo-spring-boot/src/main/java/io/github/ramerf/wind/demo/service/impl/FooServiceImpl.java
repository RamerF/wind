package io.github.ramerf.wind.demo.service.impl;

import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.service.FooService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * @author ramer
 * @since 2022.06.05
 */
@Slf4j
@Service
public class FooServiceImpl extends BaseServiceImpl<Foo, Long> implements FooService {
  public FooServiceImpl(final Dao dao) {
    super(dao);
  }
}
