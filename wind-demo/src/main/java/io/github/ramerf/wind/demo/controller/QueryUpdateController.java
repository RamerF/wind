package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Product;
import io.github.ramerf.wind.demo.entity.pojo.Product.Type;
import io.github.ramerf.wind.web.entity.response.Rs;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

/**
 * Query/Update使用示例.
 *
 * @author ramer
 * @since 2020/4/28
 */
@Slf4j
@RestController
@RequestMapping("/query-update")
@Api(tags = "Query/Update使用示例")
public class QueryUpdateController {

  @GetMapping("/create")
  @ApiOperation("使用Update,创建")
  public Rs<Product> create() {
    final Product product =
        Product.builder()
            .id(LocalDateTime.now().toString())
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .type(Type.REALITY)
            .date(new Date())
            .localDate(LocalDate.now())
            .createTime(LocalDateTime.now())
            .build();
    final Update<Product> update = Update.getInstance(Product.class);
    update.create(product);
    return Rs.ok(product);
  }

  @GetMapping("/update")
  @ApiOperation("使用Update,条件更新,指定更新字段")
  public Rs<Integer> update() {
    final Product product =
        Product.builder()
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .build();
    final LambdaCondition<Product> condition = LambdaCondition.of(Product.class);
    // 指定更新字段:title,name
    final Fields<Product> fields =
        Fields.of(Product.class).include(Product::getTitle, Product::getName);
    // 获取Update实例
    final Update<Product> update = Update.getInstance(Product.class);
    final int affectRow =
        update.where(condition.eq(Product::setId, "string-id")).update(product, fields);
    return Rs.ok(affectRow);
  }

  @GetMapping(value = "/query", params = "type=1")
  @ApiOperation("使用Query,条件组,or条件拼接")
  public Rs<List<Product>> query1() {
    final LambdaCondition<Product> condition = LambdaCondition.of(Product.class);
    // 条件组 (id='string-id' or name ='name')
    LambdaConditionGroup<Product> conditionGroup = LambdaConditionGroup.of(Product.class);
    conditionGroup.orEq(Product::setId, "string-id");
    conditionGroup.orEq(Product::setName, "name");
    // 获取Update实例
    final Query<Product> query = Query.getInstance(Product.class);
    final List<Product> products =
        query
            .select(Fields.of(Product.class))
            .where(condition.eq(Product::setId, "string-id").and(conditionGroup))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=2")
  @ApiOperation("使用Query,or拼接")
  public Rs<List<Product>> query2() {
    final LambdaCondition<Product> condition = LambdaCondition.of(Product.class);
    // 查询条件: (id='ramer' or name like 'a%')
    LambdaConditionGroup<Product> group =
        LambdaConditionGroup.of(Product.class)
            .orEq(Product::setId, "ramer")
            .orLike(Product::setName, "a%");
    condition.and(group);
    final Query<Product> query = Query.getInstance(Product.class);
    final List<Product> products =
        query.select(Fields.of(Product.class)).where(condition).fetchAll(Product.class);
    log.info("query2:[{}]", products);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=3")
  @ApiOperation("使用Query,指定字段")
  public Rs<List<IdNameResponse>> query3() {
    final LambdaCondition<Product> condition = LambdaCondition.of(Product.class);
    final Query<Product> query = Query.getInstance(Product.class);
    Fields<Product> fields = Fields.of(Product.class).include(Product::getId, Product::getName);
    final List<IdNameResponse> products =
        query
            .select(fields)
            .where(condition.like(Product::setName, "name%"))
            .fetchAll(IdNameResponse.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=4")
  @ApiOperation("使用Query,返回基本类型")
  public Rs<List<Long>> query4() {
    final LambdaCondition<Foo> condition = LambdaCondition.of(Foo.class);
    final Query<Foo> query = Query.getInstance(Foo.class);
    final List<Long> ids =
        query
            .select(Fields.of(Foo.class).include(Foo::getId))
            .where(condition.eq(Foo::setName, "name"))
            .fetchAll(Long.class);
    return Rs.ok(ids);
  }

  @GetMapping(value = "/query", params = "type=5")
  @ApiOperation("使用Query,使用StringCondition构造条件")
  public Rs<List<Product>> query5() {
    final StringCondition<Product> condition = StringCondition.of(Product.class);
    final Query<Product> query = Query.getInstance(Product.class);
    final List<Product> products =
        query
            .select(Fields.of(Product.class))
            .where(condition.eq("id", "string-id").lt("create_time", LocalDateTime.now()))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=7")
  @ApiOperation("使用Query,自定义sql查询")
  public Rs<List<Product>> query7() {
    final Query<Product> query = Query.getInstance(Product.class);
    // 单条记录
    final Product product =
        query.fetchOneBySql("select * from product where title = 'halo'", Product.class);
    // 多条记录
    final List<Product> products =
        query.fetchListBySql("select * from product where id='string-id'", Product.class);
    return Rs.ok(products);
  }

  @Data
  public static class GroupBySum {
    private BigDecimal bigDecimal;
    private String name;
  }

  @Getter
  @Setter
  public static class IdNameResponse {
    private Long id;
    private String name;
  }
}
