package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.annotation.Ds;
import io.github.ramerf.wind.core.annotation.Transactional;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import java.time.*;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 26/03/2022
 */
@Slf4j
public class FooServiceImpl extends BaseServiceImpl<Foo, Long> {
  public FooServiceImpl(final Dao dao) {
    super(dao);
  }

  @Ds("d2")
  @Transactional(rollbackFor = WindException.class)
  public void foo() {
    Foo foo = new Foo();
    foo.setName(YearMonth.now().getMonth().name());
    create(foo);
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
    if (System.currentTimeMillis() % 2 == 0) {
      throw new RuntimeException("foo fail");
    }
    foo.setName(YearMonth.now().plusMonths(1).getMonth().name());
    update(foo, Fields.of(Foo.class).include(Foo::getName));
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
  }
}
