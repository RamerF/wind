package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.condition.SortColumn.Order;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.response.FooThinResponse;
import io.github.ramerf.wind.demo.service.FooService;
import io.swagger.annotations.Api;
import java.util.Arrays;
import java.util.List;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * 该类用于辅助测试.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@Slf4j
@RestController
@RequestMapping("/test")
@Api(tags = "测试专用")
public class FooController {
  @Resource private FooService service;

  @GetMapping("/count")
  public ResponseEntity<Rs<Long>> count() {
    return Rs.ok(service.count());
  }

  @GetMapping(value = "/count", params = "type=2")
  public ResponseEntity<Rs<Long>> count2() {
    return Rs.ok(service.count(condition -> condition.like(Foo::setName, "foo")));
  }

  @GetMapping(value = "/count", params = "type=3")
  public ResponseEntity<Rs<Long>> count3() {
    return Rs.ok(
        service.count(
            query -> query.col(AbstractEntityPoJo::getId),
            condition -> condition.like(Foo::setName, "foo")));
  }

  @GetMapping("/get-by-id")
  public ResponseEntity<Rs<Foo>> getById() {
    return Rs.ok(service.getById(1L));
  }

  @GetMapping("/get-one")
  public ResponseEntity<Rs<Foo>> getOne() {
    return Rs.ok(service.getOne(condition -> condition.eq(Foo::setId, 1L)));
  }

  @GetMapping(value = "/get-one", params = "type=2")
  public ResponseEntity<Rs<FooThinResponse>> getOne2() {
    // 支持返回基本类型
    final Long one = service.getOne(query -> query.col(Foo::getId), Long.class);
    log.info("getOne2:[{}]", one);
    // 返回自定义对象
    final FooThinResponse thinResponse =
        service.getOne(query -> query.col(Foo::getId).col(Foo::getName), FooThinResponse.class);
    log.info("getOne2:[{}]", thinResponse);
    return Rs.ok(thinResponse);
  }

  @GetMapping(value = "/get-one", params = "type=3")
  public ResponseEntity<Rs<Foo>> getOne3() {
    return Rs.ok(
        service.getOne(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L)));
  }

  @GetMapping(value = "/get-one", params = "type=4")
  public ResponseEntity<Rs<Long>> getOne4() {
    return Rs.ok(
        service.getOne(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            Long.class));
  }

  @GetMapping("/list-by-ids")
  public ResponseEntity<Rs<List<Foo>>> listByIds() {
    return Rs.ok(service.listByIds(Arrays.asList(1L, 2L, 3L)));
  }

  @GetMapping("/list")
  public ResponseEntity<Rs<Long>> list() {
    return Rs.ok(service.list(condition -> condition.eq(AbstractEntityPoJo::setId, 1L)));
  }

  @GetMapping(value = "/list", params = "type=2")
  public ResponseEntity<Rs<Long>> list2() {
    return Rs.ok(service.list(condition -> condition.eq(AbstractEntityPoJo::setId, 1L)));
  }

  @GetMapping(value = "/list", params = "type=3")
  public ResponseEntity<Rs<Long>> list3() {
    return Rs.ok(
        service.list(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L)));
  }

  @GetMapping(value = "/list", params = "type=4")
  public ResponseEntity<Rs<List<Long>>> list4() {
    return Rs.ok(
        service.list(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            Long.class));
  }

  @GetMapping(value = "/list", params = "type=5")
  public ResponseEntity<Rs<List<Foo>>> list5() {
    return Rs.ok(
        service.list(
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/list", params = "type=6")
  public ResponseEntity<Rs<List<Long>>> list6() {
    return Rs.ok(
        service.list(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }

  @GetMapping(value = "/listAll")
  public ResponseEntity<Rs<List<Long>>> listAll() {
    return Rs.ok(service.listAll(query -> query.col(Foo::getId), Long.class));
  }

  @GetMapping(value = "/listAll", params = "type=2")
  public ResponseEntity<Rs<List<Foo>>> listAll2() {
    return Rs.ok(service.listAll(query -> query.col(Foo::getId), Foo.class));
  }

  @GetMapping(value = "/page")
  public ResponseEntity<Rs<Page<Foo>>> page() {
    return Rs.ok(
        service.page(
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/page", params = "type=2")
  public ResponseEntity<Rs<Page<Long>>> page2() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }

  @GetMapping(value = "/page", params = "type=3")
  public ResponseEntity<Rs<Page<Long>>> page3() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/page", params = "type=4")
  public ResponseEntity<Rs<Page<Foo>>> page4() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            condition -> condition.eq(AbstractEntityPoJo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }
}
