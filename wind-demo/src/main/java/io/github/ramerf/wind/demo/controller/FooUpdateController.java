package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.util.function.Consumer;
import javax.annotation.Resource;
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
@RequestMapping("/update")
@Api(tags = "Update使用示例")
@CrossOrigin(origins = "*", maxAge = 3600)
public class FooUpdateController {
  @Resource private PrototypeBean prototypeBean;

  @GetMapping
  @ApiOperation("使用Update")
  public ResponseEntity<Rs<Object>> update() {
    final Foo foo = Foo.builder().name("name").build();
    final int affectRow =
        prototypeBean
            .update()
            .from(Foo.class)
            .where((Consumer<ICondition<Foo>>) condition -> condition.eq(Foo::setId, 1L))
            .update(foo);
    return Rs.ok(affectRow);
  }
}
