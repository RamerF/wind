package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Product;
import io.github.ramerf.wind.demo.entity.response.IdNameResponse;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.math.BigDecimal;
import java.util.List;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
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

  @GetMapping
  @ApiOperation("使用Query")
  public Rs<List<Foo>> query() {
    // 获取查询列和查询条件对象
    final QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    final LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    // 指定查询列
    queryColumn.col(Foo::getId);
    // 指定查询条件
    condition.eq(Foo::setId, 1L);
    // 动态条件,第一个参数为false时,不会包含该条件
    condition.eq(false, Foo::setId, 2L);
    return Rs.ok(
        Query.getInstance(Foo.class).select(queryColumn).where(condition).fetchAll(Long.class));
  }

  @GetMapping(params = "type=2")
  @ApiOperation("使用Query,groupBy,sum")
  public Rs<List<GroupBySum>> query2() {
    QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final QueryEntityMetaData<Foo> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    final GroupByClause<Foo> clause = queryEntityMetaData.getGroupByClause();
    final List<GroupBySum> list =
        Query.getInstance(Foo.class)
            .select(queryColumn.sum(Foo::getId, "big_decimal").col(Foo::getName, "name"))
            .where(condition)
            .groupBy(clause.col(Foo::getName))
            .fetchAll(GroupBySum.class);
    return Rs.ok(list);
  }

  @GetMapping("/diy")
  @ApiOperation("使用Query,DIY)")
  public Rs<List<Foo>> query3() {
    /* 说明:只支持inner join方式连表 */
    // 获取查询列和查询条件对象
    final QueryColumn<Foo> fooQueryColumn = QueryColumn.fromClass(Foo.class);
    final QueryColumn<Product> accountQueryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Foo> fooCondition = LambdaCondition.getInstance(fooQueryColumn);
    // 指定查询列
    fooQueryColumn.col(Foo::getId);
    // 指定查询条件
    fooCondition.eq(Foo::setId, 1L);
    // 执行连表: foo.id=account.id
    fooCondition.eq(Foo::getId, accountQueryColumn, Product::getId);
    final LambdaCondition<Product> accountCondition = LambdaCondition.getInstance(accountQueryColumn);
    accountCondition.eq(Product::setId, "1");
    return Rs.ok(
        Query.getInstance(Foo.class)
            .select(fooQueryColumn)
            .where(fooCondition, accountCondition)
            .fetchAll(Foo.class));
  }

  @GetMapping(value = "/diy", params = "type=2")
  @ApiOperation("使用Query,查询任意表)")
  public Rs<List<IdNameResponse>> query4() {
    // 获取查询列和查询条件对象
    final QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    final LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final List<IdNameResponse> list =
        Query.getInstance(Foo.class)
            .select(queryColumn.col(Foo::getId).col(Foo::getName))
            .where(condition)
            .fetchAll(IdNameResponse.class);
    return Rs.ok(list);
  }

  @Data
  public static class GroupBySum {
    private BigDecimal bigDecimal;
    private String name;
  }
}
