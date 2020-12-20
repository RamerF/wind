package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.executor.Query;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.service.InterService.Fields;
import io.github.ramerf.wind.demo.entity.pojo.Foo;
import io.github.ramerf.wind.demo.entity.pojo.Product;
import io.github.ramerf.wind.demo.entity.pojo.Product.Type;
import io.github.ramerf.wind.demo.entity.response.IdNameResponse;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

/**
 * Query/Update使用示例.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@Slf4j
@RestController
@RequestMapping("/query-update")
@Api(tags = "Query/Update使用示例")
public class QueryUpdateController {
  @Resource private PrototypeBean prototypeBean;

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
    final Update<Product> update = prototypeBean.update(Product.class);
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
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    // 指定更新字段:title,name
    final Fields<Product> fields =
        Fields.with(Product.class).include(Product::getTitle, Product::getName);
    // 获取Update实例
    final Update<Product> update = prototypeBean.update(Product.class);
    final int affectRow =
        update.where(condition.eq(Product::setId, "string-id")).update(product, fields);
    return Rs.ok(affectRow);
  }

  @GetMapping(value = "/query", params = "type=1")
  @ApiOperation("使用Query,条件组,or条件拼接")
  public Rs<List<Product>> query1() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final CustomCondition<Product> condition = CustomCondition.getInstance(queryColumn);
    // 条件组 (id='string-id' or name ='name')
    LambdaConditionGroup<Product> conditionGroup = LambdaConditionGroup.getInstance(queryColumn);
    conditionGroup.orEq(Product::setId, "string-id");
    conditionGroup.orEq(Product::setName, "name");
    // 获取Update实例
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.notEq(Product::setId, "string-id").and(conditionGroup))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=2")
  @ApiOperation("使用Query,or拼接")
  public Rs<List<Product>> query2() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    // 查询条件: (id='ramer' and name like 'a%') or (id='jerry' and name like 'b%')
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    condition
        .and(group -> group.andEq(Product::setId, "ramer").andLike(Product::setName, "a%"))
        .or(group -> group.andEq(Product::setId, "jerry").andLike(Product::setName, "b%"));
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query.select(queryColumn).where(condition).fetchAll(Product.class);
    log.info("query2:[{}]", products);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=3")
  @ApiOperation("使用Query,指定字段")
  public Rs<List<IdNameResponse>> query3() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<IdNameResponse> products =
        query
            .select(queryColumn.col(Product::getId).col(Product::getName))
            .where(condition.like(Product::setName, "name%"))
            .fetchAll(IdNameResponse.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=4")
  @ApiOperation("使用Query,返回基本类型")
  public Rs<List<Long>> query4() {
    final QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    final LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final Query<Foo> query = prototypeBean.query(Foo.class);
    final List<Long> ids =
        query
            .select(queryColumn.col(Foo::getId))
            .where(condition.eq(Foo::setName, "name"))
            .fetchAll(Long.class);
    return Rs.ok(ids);
  }

  @GetMapping(value = "/query", params = "type=5")
  @ApiOperation("使用Query,使用StringCondition构造条件")
  public Rs<List<Product>> query5() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final StringCondition<Product> condition = StringCondition.getInstance(queryColumn);
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.eq("id", "string-id").lt("create_time", LocalDateTime.now()))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  @GetMapping(value = "/query", params = "type=6")
  @ApiOperation("使用Query,自定义sql查询)")
  public Rs<List<Product>> query6() {
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    final LambdaCondition<Product> condition = LambdaCondition.getInstance(queryColumn);
    // 执行连表: foo.id=account.id
    condition.eq(Product::getId, queryColumn, Product::getId);
    final Query<Product> query = Query.getInstance(Product.class);
    queryColumn.col(
        "(case title when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) title,id");
    return Rs.ok(query.select(queryColumn).where(condition.and("id<>'1'")).fetchAll(Product.class));
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
        query.fetchAllBySql("select * from product where id='string-id'", Product.class);
    return Rs.ok(products);
  }

  /** 不建议使用该操作,如果是复杂的sql,建议直接使用sql查询 {@link #query6()},{@link #query7()} */
  @GetMapping(value = "/query", params = "type=8")
  @ApiOperation("使用Query,groupBy,sum")
  public Rs<List<GroupBySum>> query8() {
    QueryColumn<Foo> queryColumn = QueryColumn.fromClass(Foo.class);
    LambdaCondition<Foo> condition = LambdaCondition.getInstance(queryColumn);
    final QueryEntityMetaData<Foo> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    final GroupByClause<Foo> clause = queryEntityMetaData.getGroupByClause();
    final Query<Foo> query = prototypeBean.query(Foo.class);
    final List<GroupBySum> list =
        query
            .select(queryColumn.sum(Foo::getId, "big_decimal").col(Foo::getName, "name"))
            .where(condition)
            .groupBy(clause.col(Foo::getName))
            .fetchAll(GroupBySum.class);
    return Rs.ok(list);
  }

  @GetMapping(value = "/extend-condition")
  @ApiOperation("使用Query,返回任意对象)")
  public Rs<List<Product>> extendCondition() {
    // 可指定查询列
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    // 扩展Condition
    final CustomCondition<Product> condition = CustomCondition.getInstance(queryColumn);
    // 获取Query实例
    final Query<Product> query = prototypeBean.query(Product.class);
    final List<Product> products =
        query
            .select(queryColumn)
            .where(condition.notEq(Product::setId, "string-id"))
            .fetchAll(Product.class);
    return Rs.ok(products);
  }

  /** 示例:扩展条件{@link Condition}. */
  public static class CustomCondition<T extends AbstractEntityPoJo<T, ?>>
      extends AbstractCondition<T> {
    public CustomCondition(final QueryColumn<T> queryColumn) {
      super(queryColumn);
    }

    public CustomCondition(final Class<T> clazz, final String tableName, final String tableAlia) {
      super(clazz, tableName, tableAlia);
    }

    public static <T extends AbstractEntityPoJo<T, ?>> CustomCondition<T> getInstance(
        final QueryColumn<T> queryColumn) {
      return new CustomCondition<>(queryColumn);
    }

    public <V> CustomCondition<T> notEq(@Nonnull final IConsumer<T, V> field, final V value) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator() : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator())
              .concat(field.getColumn())
              .concat("<>")
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
      return this;
    }

    public CustomCondition<T> and(@Nonnull LambdaConditionGroup<T> group) {
      if (group.getCondition().getValueTypes().size() > 0) {
        conditionSql.add(
            (conditionSql.size() > 0 ? AND.operator() : "")
                .concat(PARENTHESIS_FORMAT.format(group.getCondition().getString())));
        valueTypes.addAll(group.getCondition().getValueTypes());
      }
      return this;
    }
  }

  @Data
  public static class GroupBySum {
    private BigDecimal bigDecimal;
    private String name;
  }
}
