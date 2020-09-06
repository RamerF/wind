package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.OoO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.math.BigDecimal;
import java.util.List;
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
@RequestMapping("/query")
@Api(tags = "Query使用示例")
@CrossOrigin(origins = "*", maxAge = 3600)
public class FooQueryController {
  @Resource private PrototypeBean prototypeBean;

  @GetMapping
  @ApiOperation("使用Query")
  public ResponseEntity<Rs<List<Long>>> query() {
    // 获取查询列和查询条件对象
    final QueryColumn<Foo> queryColumn = QueryColumnFactory.fromClass(Foo.class);
    final Condition<Foo> condition = queryColumn.getCondition();
    // 指定查询列
    queryColumn.col(Foo::getId);
    // 指定查询条件
    condition.eq(Foo::setId, 1L);
    // 动态条件,第一个参数为false时,不会包含该条件
    condition.eq(false, Foo::setId, 2L);
    return Rs.ok(prototypeBean.query().select(queryColumn).where(condition).fetchAll(Long.class));
  }

  @GetMapping(params = "type=2")
  @ApiOperation("使用Query,groupBy,sum")
  public ResponseEntity<Rs<List<GroupBySum>>> query2() {
    QueryColumn<Foo> queryColumn = QueryColumnFactory.fromClass(Foo.class);
    Condition<Foo> condition = queryColumn.getCondition().gt(Foo::setId, 0L);
    final QueryEntityMetaData<Foo> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    final GroupByClause<Foo> clause = queryEntityMetaData.getGroupByClause();
    final List<GroupBySum> list =
        prototypeBean
            .query()
            .select(queryColumn.sum(Foo::getId, "big_decimal").col(Foo::getName, "name"))
            .where(condition)
            .groupBy(clause.col(Foo::getName))
            .fetchAll(GroupBySum.class);
    return Rs.ok(list);
  }

  @GetMapping("/diy")
  @ApiOperation("使用Query,DIY)")
  public ResponseEntity<Rs<List<Foo>>> query3() {
    /*
     * 说明: 由于设计用于分布式系统,应避免多表查询,故设计时未考虑支持🤣🤣,只支持inner join方式连表
     * 已知的问题,在Query中已经标明(但不完全😅,没想到吧)
     */

    // 获取查询列和查询条件对象
    final QueryColumn<Foo> queryColumn = QueryColumnFactory.fromClass(Foo.class);
    final QueryColumn<OoO> queryColumn2 = QueryColumnFactory.fromClass(OoO.class);
    final Condition<Foo> condition = queryColumn.getCondition();
    // 指定查询列
    queryColumn.col(Foo::getId);
    // 指定查询条件
    condition.eq(Foo::setId, 1L);
    // 执行连表: foo.id=o_o.id
    condition.eq(Foo::getId, queryColumn2, OoO::getId);
    return Rs.ok(prototypeBean.query().select(queryColumn).where(condition).fetchAll(Foo.class));
  }

  @Data
  public static class GroupBySum {
    private BigDecimal bigDecimal;
    private String name;
  }
}
