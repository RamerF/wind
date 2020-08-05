package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.condition.SortColumn.Order;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.helper.ControllerHelper;
import io.github.ramerf.wind.core.util.StringUtils;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.request.FooRequest;
import io.github.ramerf.wind.demo.entity.response.FooResponse;
import io.github.ramerf.wind.demo.entity.response.ResCode;
import io.github.ramerf.wind.demo.service.FooService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.util.List;
import javax.annotation.Resource;
import javax.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;

/**
 * 该类用于辅助测试.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@Slf4j
@RestController
@RequestMapping("/foo")
@SuppressWarnings("unchecked")
@Api(tags = "controller层方法使用示例")
@CrossOrigin(origins = "*", maxAge = 3600)
public class FooController {
  @Resource private FooService service;

  @PostMapping("/create")
  @ApiOperation("创建,创建poJo,自定义错误信息")
  public ResponseEntity<Rs<Object>> create(
      @Valid @RequestBody final FooRequest fooRequest, final BindingResult bindingResult) {
    if (bindingResult.hasErrors()) {
      return Rs.fail(ControllerHelper.collectBindingResult(bindingResult));
    }
    return ControllerHelper.create(service, fooRequest.poJo(), ResultCode.ERROR);
  }

  @PostMapping(value = "/create", params = "type=2")
  @ApiOperation("创建,创建poJo")
  public ResponseEntity<Rs<Object>> create2(
      @Valid @RequestBody final FooRequest fooRequest, final BindingResult bindingResult) {
    return ControllerHelper.create(service, fooRequest.poJo(), bindingResult);
  }

  @PostMapping(value = "/create", params = "type=3")
  @ApiOperation("创建,指定保存null属性")
  public ResponseEntity<Rs<Object>> create3(
      @Valid @RequestBody final FooRequest fooRequest, final BindingResult bindingResult) {
    return ControllerHelper.create(
        service, fooRequest, bindingResult, Foo::getTextString, Foo::getName);
  }

  @GetMapping(value = "/detail/{id}")
  @ApiOperation("查询,根据id获取详情")
  public ResponseEntity<Rs<Object>> detail(@PathVariable("id") final long id) {
    return ControllerHelper.detail(service, id);
  }

  @GetMapping(value = "/detail/{id}", params = "type=2")
  @ApiOperation("查询,根据id获取详情,并转换为response")
  public ResponseEntity<Rs<FooResponse>> detail2(@PathVariable("id") final long id) {
    /// 🤔🤔🤔🤔🤔🤔😉
    // ControllerHelper.detail(
    //     service,
    //     id,
    //     foo -> {
    //       // foo为方法service.getById的返回值,可以在这里组装业务返回对象
    //       log.info("detail2:[{}]", foo);
    //       return foo;
    //     });
    return ControllerHelper.detail(service, id, FooResponse::of);
  }

  @GetMapping(value = "/list")
  @ApiOperation("查询,列表查询,支持转换和过滤")
  public ResponseEntity<Rs<List<FooResponse>>> list() {
    // page需要自己调用分页查询,仅提供相关的对象转换方法
    final List<Foo> list = service.list(condition -> condition.like(Foo::setName, "foo"));
    return ControllerHelper.list(list, FooResponse::of, foo -> StringUtils.nonEmpty(foo.getName()));
  }

  @GetMapping(value = "/page")
  @ApiOperation("查询,分页")
  public ResponseEntity<Rs<Page<FooResponse>>> page() {
    // page需要自己调用分页查询,仅提供相关的对象转换方法
    final Page<Foo> page =
        service.page(
            condition -> condition.eq(Foo::setName, "foo"),
            1,
            10,
            SortColumn.by(Foo::getUpdateTime, Order.DESC));
    return ControllerHelper.page(page, FooResponse::of);
  }

  @GetMapping(value = "/page", params = "type=2")
  @ApiOperation("查询,列表查询,转换为分页对象")
  public ResponseEntity<Rs<Page<FooResponse>>> page2() {
    // page需要自己调用分页查询,仅提供相关的对象转换方法
    final List<Foo> list = service.list(condition -> condition.eq(Foo::setName, "foo"));
    return ControllerHelper.page(list, FooResponse::of, foo -> StringUtils.nonEmpty(foo.getName()));
  }

  @PostMapping(value = "/update/{id}")
  @ApiOperation("更新,更新request")
  public ResponseEntity<Rs<String>> update(
      @PathVariable("id") final long id,
      @RequestBody FooRequest fooRequest,
      final BindingResult bindingResult) {
    // 收集校验错误信息
    if (bindingResult.hasErrors()) {
      return Rs.fail(ControllerHelper.collectFirstBindingResult(bindingResult));
    }
    // 获取对应的poJo,处理其它业务逻辑
    final Foo foo = fooRequest.poJo(id);
    return ControllerHelper.update(service, foo, id, bindingResult);
  }

  @PostMapping(value = "/update/{id}", params = "type=2")
  @ApiOperation("更新,直接更新PoJo")
  public ResponseEntity<Rs<String>> update2(
      @PathVariable("id") final long id,
      @RequestBody FooRequest fooRequest,
      final BindingResult bindingResult) {
    // 收集校验错误信息
    if (bindingResult.hasErrors()) {
      return Rs.fail(ControllerHelper.collectFirstBindingResult(bindingResult));
    }
    // 获取对应的poJo,处理其它业务逻辑
    final Foo foo = fooRequest.poJo(id);
    return ControllerHelper.update(service, foo);
  }

  @PostMapping(value = "/update/{id}", params = "type=3")
  @ApiOperation("更新,直接更新PoJo,自定义执行失败时的返回信息")
  public ResponseEntity<Rs<String>> update3(
      @PathVariable("id") final long id,
      @RequestBody FooRequest fooRequest,
      final BindingResult bindingResult) {
    // 收集校验错误信息
    if (bindingResult.hasErrors()) {
      return Rs.fail(ControllerHelper.collectFirstBindingResult(bindingResult));
    }
    // 获取对应的poJo,处理其它业务逻辑
    final Foo foo = fooRequest.poJo(id);
    return ControllerHelper.update(service, foo, ResCode.FOO_FAIL_UPDATE);
  }

  @PostMapping(value = "/update/{id}", params = "type=4")
  @ApiOperation("更新,直接更新PoJo,自定义执行成功和失败时的返回信息")
  public ResponseEntity<Rs<String>> update4(
      @PathVariable("id") final long id,
      @RequestBody FooRequest fooRequest,
      final BindingResult bindingResult) {
    // 收集校验错误信息
    if (bindingResult.hasErrors()) {
      return Rs.fail(ControllerHelper.collectFirstBindingResult(bindingResult));
    }
    // 获取对应的poJo,处理其它业务逻辑
    final Foo foo = fooRequest.poJo(id);
    return ControllerHelper.update(
        service, foo, ResCode.FOO_SUCCESS_UPDATE, ResCode.FOO_FAIL_UPDATE);
  }

  @PostMapping(value = "/update/{id}", params = "type=5")
  @ApiOperation("更新,指定保存列")
  public ResponseEntity<Rs<Object>> update5(
      @PathVariable("id") final long id,
      @RequestBody FooRequest fooRequest,
      final BindingResult bindingResult) {
    return ControllerHelper.update(service, fooRequest, id, bindingResult, Foo::getName);
  }

  @PostMapping(value = "/delete/{id}")
  @ApiOperation("删除,根据id删除")
  public ResponseEntity<Rs<Object>> delete(@PathVariable("id") final long id) {
    return ControllerHelper.delete(service, id);
  }

  @PostMapping(value = "/delete/{id}", params = "type=2")
  @ApiOperation("删除,自定义删除,不带返回值")
  public ResponseEntity<Rs<String>> delete2(@PathVariable("id") final long id) {
    return ControllerHelper.delete(
        () -> service.delete(id),
        () -> {
          // 这里处理业务逻辑,成功时的返回信息
          return Rs.ok(ResCode.FOO_SUCCESS_DELETE);
        },
        ResCode.FOO_FAIL_DELETE);
  }

  @PostMapping(value = "/delete/{id}", params = "type=3")
  @ApiOperation("删除,自定义删除,带返回值")
  public ResponseEntity<Rs<String>> delete3(@PathVariable("id") final long id) {
    return ControllerHelper.delete(
        service.delete(condition -> condition.eq(Foo::setId, id)),
        result -> {
          // 这里处理业务逻辑,成功时的返回信息
          return Rs.ok(ResCode.FOO_SUCCESS_DELETE);
        },
        ResCode.FOO_FAIL_DELETE);
  }

  @PostMapping(value = "/deleteByIds")
  @ApiOperation("删除,根据id批量删除")
  public ResponseEntity<Rs<String>> deleteByIds(@RequestParam("ids") final List<Long> ids) {
    return ControllerHelper.deleteByIds(service, ids);
  }
}
