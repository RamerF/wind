package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.condition.SortColumn.Order;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Foo.Type;
import io.github.ramerf.wind.demo.service.FooService;
import io.github.ramerf.wind.web.entity.response.Rs;
import io.swagger.annotations.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Resource;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.web.bind.annotation.*;

import static java.util.stream.Collectors.toList;

/**
 * service层方法使用示例.
 *
 * @author ramer
 * @since 2020/4/28
 */
@SuppressWarnings("DuplicatedCode")
@Slf4j
@RestController
@RequestMapping("/service")
@Api(tags = "service层方法使用示例")
public class ServiceController {
  @Resource private FooService service;
  private static final Foo foo =
      Foo.builder()
          .name("demo")
          .textString("text")
          .bigDecimal(BigDecimal.valueOf(100))
          .type(Type.SPORT)
          .intList(Arrays.asList(1, 3, 5))
          .intArr(new Integer[] {1, 4, 7})
          .longList(Arrays.asList(2L, 4L, 6L))
          .longArr(new Long[] {1L, 3L, 5L})
          // .stringList(Arrays.asList("3", "a", "6", "b"))
          .stringArr(new String[] {"2", "a", "b"})
          .column("non_match_column")
          .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
          .build();

  @GetMapping("/generic-service")
  @ApiOperation("通用service")
  public Rs<Long> genericService() {
    return Rs.ok(GenericService.with(Foo.class, Long.class).count());
  }

  @GetMapping("/count")
  @ApiOperation("count,所有(不包含已删除)")
  public Rs<Long> count() {
    return Rs.ok(service.count());
  }

  @GetMapping(value = "/count", params = "type=2")
  @ApiOperation("count,条件count")
  public Rs<Long> count2() {
    return Rs.ok(service.count(condition -> condition.like(Foo::setName, "foo")));
  }

  @GetMapping(value = "/count", params = "type=3")
  @ApiOperation("count,指定返回列")
  public Rs<Long> count3() {
    return Rs.ok(
        service.count(
            query -> query.col(Foo::getId), condition -> condition.like(Foo::setName, "foo")));
  }

  @GetMapping("/get-by-id")
  @ApiOperation("查询,单条查询,根据id获取poJo")
  public Rs<Foo> getById() {
    return Rs.ok(service.getById(1L));
  }

  @GetMapping("/get-one")
  @ApiOperation("查询,单条查询,指定条件获取poJo")
  public Rs<Foo> getOne() {
    return Rs.ok(service.getOne(condition -> condition.eq(Foo::setId, 1L)));
  }

  @GetMapping(value = "/get-one", params = "type=2")
  @ApiOperation("查询,单条查询,指定条件返回自定义对象,支持返回基本类型")
  public Rs<FooThinResponse> getOne2() {
    // 支持返回基本类型,支持枚举
    final Long one =
        service.getOne(
            query -> query.col(Foo::getId), condition -> condition.eq(Foo::setId, 1L), Long.class);
    log.info("getOne2:[{}]", one);
    // 返回自定义对象
    final FooThinResponse thinResponse =
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName).col(Foo::getCreateTime),
            condition -> condition.eq(Foo::setId, 1L),
            FooThinResponse.class);
    log.info("getOne2:[{}]", thinResponse);
    return Rs.ok(thinResponse);
  }

  @GetMapping(value = "/get-one", params = "type=3")
  @ApiOperation("查询,单条查询,指定条件,指定返回列")
  public Rs<Foo> getOne3() {
    return Rs.ok(
        service.getOne(
            query ->
                query
                    .col(Foo::getId)
                    .col(Foo::getName)
                    .col(Foo::getCreateTime)
                    .col(Foo::getUpdateTime),
            condition -> condition.eq(Foo::setId, 1L)));
  }

  @GetMapping(value = "/get-one", params = "type=4")
  @ApiOperation("查询,单条查询,指定条件,指定返回列,返回指定对象")
  public Rs<Long> getOne4() {
    return Rs.ok(
        service.getOne(
            query -> query.col(Foo::getId), condition -> condition.eq(Foo::setId, 1L), Long.class));
  }

  @GetMapping(value = "/get-one", params = "type=5")
  @ApiOperation("查询,单条查询,自定义sql")
  public Rs<Foo> getOne5() {
    return Rs.ok(service.getOne("select * from foo limit 1", Foo.class));
  }

  @GetMapping("/list-by-ids")
  @ApiOperation("查询,列表查询,通过id集合查询poJo")
  public Rs<List<Foo>> listByIds() {
    return Rs.ok(service.listByIds(Arrays.asList(1L, 2L, 3L)));
  }

  @GetMapping("/list")
  @ApiOperation("查询,列表查询,指定条件")
  public Rs<List<Foo>> list() {
    return Rs.ok(service.list(condition -> condition.eq(Foo::setId, 1L)));
  }

  @GetMapping(value = "/list", params = "type=2")
  @ApiOperation("查询,列表查询,指定返回列,返回自定义对象")
  public Rs<List<Long>> list2() {
    return Rs.ok(service.list(query -> query.col(Foo::getId), Long.class));
  }

  @GetMapping(value = "/list", params = "type=3")
  @ApiOperation("查询,列表查询,指定条件,指定返回列")
  public Rs<List<Foo>> list3() {
    return Rs.ok(
        service.list(query -> query.col(Foo::getId), condition -> condition.eq(Foo::setId, 1L)));
  }

  @GetMapping(value = "/list", params = "type=4")
  @ApiOperation("查询,列表查询,指定条件,指定返回列,返回自定义对象")
  public Rs<List<Long>> list4() {
    return Rs.ok(
        service.list(
            query -> query.col(Foo::getId), condition -> condition.eq(Foo::setId, 1L), Long.class));
  }

  @GetMapping(value = "/list", params = "type=5")
  @ApiOperation("查询,列表查询,获取指定页的列表数据")
  public Rs<List<Foo>> list5() {
    return Rs.ok(
        service.list(
            condition -> condition.eq(Foo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/list", params = "type=6")
  @ApiOperation("查询,列表查询,获取指定页的列表数据,指定排序规则")
  public Rs<List<Long>> list6() {
    return Rs.ok(
        service.list(
            query -> query.col(Foo::getId),
            condition -> condition.eq(Foo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }

  @GetMapping(value = "/list", params = "type=7")
  @ApiOperation("查询,列表查询,自定义sql")
  public Rs<List<Foo>> list7() {
    return Rs.ok(
        service.list(
            query ->
                query.col(
                    "(case title when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) title,id"),
            condition -> condition.eq(Foo::setId, 1L).and("title is not null")));
  }

  @GetMapping(value = "/list", params = "type=8")
  @ApiOperation("查询,列表查询,自定义sql")
  public Rs<List<Foo>> list8() {
    return Rs.ok(service.listAll("select * from foo", Foo.class));
  }

  @GetMapping(value = "/listAll")
  @ApiOperation("查询,列表查询,所有记录,指定返回列,返回自定义对象")
  public Rs<List<Long>> listAll() {
    return Rs.ok(service.listAll(query -> query.col(Foo::getId), Long.class));
  }

  @GetMapping(value = "/listAll", params = "type=2")
  @ApiOperation("查询,列表查询,所有记录,指定返回列,返回自定义对象")
  public Rs<List<Foo>> listAll2() {
    return Rs.ok(service.listAll(query -> query.col(Foo::getId), Foo.class));
  }

  @GetMapping(value = "/listAll", params = "type=3")
  @ApiOperation("查询,自定义sql")
  public Rs<List<Foo>> listAll3() {
    return Rs.ok(service.listAll("select * from foo", Foo.class));
  }

  @GetMapping(value = "/page")
  @ApiOperation("查询,分页查询,指定条件,指定排序规则")
  public Rs<Page<Foo>> page() {
    return Rs.ok(
        service.page(
            condition -> condition.eq(Foo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/page", params = "type=2")
  @ApiOperation("查询,分页查询,指定返回列,指定排序规则,返回自定义对象")
  public Rs<Page<Long>> page2() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }

  @GetMapping(value = "/page", params = "type=3")
  @ApiOperation("查询,分页查询,指定返回列,指定条件,指定排序规则")
  public Rs<Page<Foo>> page3() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            condition -> condition.eq(Foo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime)));
  }

  @GetMapping(value = "/page", params = "type=4")
  @ApiOperation("查询,分页查询,指定返回列,指定条件,指定排序规则,返回自定义对象")
  public Rs<Page<Long>> page4() {
    return Rs.ok(
        service.page(
            query -> query.col(Foo::getId),
            condition -> condition.eq(Foo::setId, 1L),
            1,
            10,
            SortColumn.by(Foo::getCreateTime, Order.DESC).asc(Foo::getUpdateTime),
            Long.class));
  }

  @PostMapping(value = "/create")
  @ApiOperation("创建")
  public Rs<Foo> create() {
    return Rs.ok(service.create(foo));
  }

  @PostMapping(value = "/create", params = "type=2")
  @ApiOperation("创建,指定属性")
  public Rs<Foo> create2() {
    return Rs.ok(service.create(foo, fields -> fields.include(Foo::getName)));
  }

  @PostMapping(value = "/create", params = "type=3")
  @ApiOperation("创建,域对象")
  public Rs<Foo> create3() {
    return Rs.ok(foo.create());
  }

  @PostMapping(value = "/createBatch")
  @ApiOperation("创建,批量创建,指定属性")
  public Rs<Integer> createBatch() {
    final List<Foo> list =
        LongStream.range(0, 100)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("demo" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    /// 默认不保存值为null的属性,下面是保存null的属性
    // final int batch = service.createBatchWithNull(list, Arrays.asList(Foo::getName,
    // Foo::getStringList));
    service.createBatch(list);
    final int batch = list.size();
    log.info("createBatch:[total:{},time elapse:{}]", batch, (System.currentTimeMillis() - start));
    return Rs.ok(batch);
  }

  @PostMapping(value = "/update")
  @ApiOperation("更新")
  public Rs<Integer> update1() {
    foo.setId(1L);
    foo.setName("<" + LocalDateTime.now() + ">");
    return Rs.ok(service.update(foo));
  }

  @PostMapping(value = "/update", params = "type=2")
  @ApiOperation("更新,指定属性")
  public Rs<Integer> update2() {
    foo.setId(1L);
    foo.setName("<" + LocalDateTime.now() + ">");
    return Rs.ok(service.update(foo, fields -> fields.include(Foo::getName)));
  }

  @PostMapping(value = "/update", params = "type=3")
  @ApiOperation("更新,条件更新")
  public Rs<Integer> update3() {
    foo.setName("<" + LocalDateTime.now() + ">");
    return Rs.ok(
        service.updateByCondition(
            foo, condition -> condition.eq(Foo::setId, 1L).isNotNull(Foo::setCreateTime)));
  }

  @PostMapping(value = "/updateBatch")
  @ApiOperation("批量更新,指定属性,自定义未预期的返回结果操作")
  public Rs<Long> updateBatch() {
    final List<Foo> list =
        LongStream.range(0, 100)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("demo" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    service
        .updateBatch(list)
        // 只有当list不为空且更新记录数和list的大小不同时,才会执行下方的代码,入参为实际受影响的行数
        .ifPresent(affectRow -> log.info("updateBatch:affectRow[{}]", affectRow));
    log.info(
        "updateBatch:[total:{},time elapse:{}]", list.size(), (System.currentTimeMillis() - start));
    return Rs.ok();
  }

  @PostMapping(value = "/updateBatch", params = "type=2")
  @ApiOperation("批量更新,指定属性,自定义未预期的返回结果操作")
  public Rs<Long> updateBatch2() {
    final List<Foo> list =
        LongStream.range(0, 100)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("demo" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    service
        .updateBatch(list, fields -> fields.include(Foo::getName))
        // 只有当list不为空且更新记录数和list的大小不同时,才会执行下方的代码,入参为实际受影响的行数
        .ifPresent(affectRow -> log.info("updateBatch:affectRow[{}]", affectRow));
    log.info(
        "updateBatch:[total:{},time elapse:{}]", list.size(), (System.currentTimeMillis() - start));
    return Rs.ok();
  }

  @GetMapping(value = "/delete")
  @ApiOperation("删除")
  public Rs<Integer> delete() {
    return Rs.ok(service.delete(1L));
  }

  @GetMapping(value = "/delete", params = "type=2")
  @ApiOperation("删除,条件删除")
  public Rs<Long> delete2() {
    final long affectRow = service.delete(condition -> condition.eq(Foo::setId, 1L));
    log.info("delete2:[{}]", affectRow);
    return Rs.ok(affectRow);
  }

  @GetMapping(value = "/deleteByIds")
  @ApiOperation("批量删除")
  public Rs<Long> deleteByIds() {
    service
        .deleteByIds(Arrays.asList(1L, 2L, 3L, 4L))
        // 只有当ids不为空且删除记录数和ids的大小不同时,才会执行下方的代码,入参为实际受影响的行数
        .ifPresent(affectRow -> log.info("deleteByIds:[{}]", affectRow));
    return Rs.ok();
  }

  @Data
  @AllArgsConstructor
  @NoArgsConstructor
  @ToString(callSuper = true)
  @ApiModel(value = "Foo只包含部分属性")
  public static class FooThinResponse {

    @ApiModelProperty(value = "String", example = "示例值")
    private String name;
  }
}
