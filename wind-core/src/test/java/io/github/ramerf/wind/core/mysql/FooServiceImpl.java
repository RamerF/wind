package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.annotation.Ds;
import io.github.ramerf.wind.core.annotation.Transactional;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.service.BaseServiceImpl;
import java.time.LocalDateTime;
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

  @Ds("d1")
  @Transactional(rollbackFor = WindException.class)
  public void foo() {
    log.info("foo:[{}]", ">>>>>>>>>>>>>>>>>>>>d1 start>>>>>>>>>>>>>>>>>>>>");
    Foo foo = new Foo();
    foo.setName("create-" + LocalDateTime.now());
    create(foo);
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
    if (System.currentTimeMillis() % 2 == 0) {
      // throw new RuntimeException("foo not rollback: " + foo.getId() + "-" + foo.getName());
    }
    foo.setName("update-" + LocalDateTime.now());
    update(foo, Fields.of(Foo.class).include(Foo::getName));
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
    log.info("foo:[{}]", ">>>>>>>>>>>>>>>>>>>>d1 end>>>>>>>>>>>>>>>>>>>>");
    // foo2();
  }

  @Ds("d2")
  @Transactional(rollbackFor = Exception.class)
  public void foo2() {
    log.info("foo:[{}]", ">>>>>>>>>>>>>>>>>>>>d2 start>>>>>>>>>>>>>>>>>>>>");
    Foo foo = new Foo();
    foo.setName("create-" + LocalDateTime.now());
    create(foo);
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
    if (System.currentTimeMillis() % 2 == 0) {
      // throw new RuntimeException("foo rollback: " + foo.getId() + "-" + foo.getName());
    }
    foo.setName("update-" + LocalDateTime.now());
    update(foo, Fields.of(Foo.class).include(Foo::getName));
    log.info("foo:[{},{}]", foo.getId(), foo.getName());
    log.info("foo:[{}]", ">>>>>>>>>>>>>>>>>>>>d2 end>>>>>>>>>>>>>>>>>>>>");
  }
}
