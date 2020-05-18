package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Type;
import io.github.ramerf.wind.demo.service.FooService;
import io.swagger.annotations.Api;
import java.math.BigDecimal;
import javax.annotation.Resource;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
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
@SuppressWarnings("all")
public class DemoProductController {
  @Resource private FooService service;

  @GetMapping(params = "type=1")
  public ResponseEntity<Rs<Object>> foo1() {
    return Rs.ok();
  }

  @GetMapping(params = "type=2")
  public ResponseEntity<Rs<Object>> foo2() {
    return Rs.ok("one");
  }

  @GetMapping(params = "type=3")
  public ResponseEntity<Rs<Object>> foo3() {
    return Rs.ok(service.lists(condition -> condition.eq(Foo::setType, Type.PHONE)));
  }

  @Data
  public static class Ts {
    private BigDecimal bigDecimal;
    private String name2;
  }
}
