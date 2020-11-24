package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.executor.Update;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.service.UpdateService.Fields;
import io.github.ramerf.wind.demo.entity.pojo.Product;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import java.time.LocalDateTime;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.AND;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.DOT;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;

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

  @GetMapping("/create")
  @ApiOperation("创建")
  public Rs<Integer> create() {
    final Product product =
        Product.builder()
            .id(LocalDateTime.now().toString())
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .build();
    final Update<Product> update = prototypeBean.update(Product.class);
    final Product created = update.create(product);
    return Rs.ok(created);
  }

  @GetMapping("/update")
  @ApiOperation("更新")
  public Rs<Integer> update() {
    final Product product =
        Product.builder()
            .id(LocalDateTime.now().toString())
            .name("name" + LocalDateTime.now())
            .title("title" + LocalDateTime.now())
            .build();

    // 可指定查询列
    final QueryColumn<Product> queryColumn = QueryColumn.fromClass(Product.class);
    // 可指定查询条件
    // final ConditionCustom<Product> condition = ConditionCustom.getInstance(queryColumn);
    final StringCondition<Product> condition = StringCondition.getInstance(queryColumn);
    // 指定仅更新title字段
    final Fields<Product> fields = Fields.with(Product.class).include(Product::getTitle);
    // 获取Update实例
    final Update<Product> update = prototypeBean.update(Product.class);
    final int affectRow =
        update
            // .where(condition.notEq(Product::setId, "2020-11-23T15:19:55.595"))
            .where(condition.and("id", "=", "2020-11-23T15:19:55.595"))
            .update(product, fields);
    return Rs.ok(affectRow);
  }

  /** 示例:自定义条件{@link Condition},可用于扩展. */
  public static class ConditionCustom<T extends AbstractEntityPoJo<T, ?>>
      extends AbstractCondition<T> {
    public ConditionCustom(final QueryColumn<T> queryColumn) {
      super(queryColumn);
    }

    public ConditionCustom(final Class<T> clazz, final String tableName, final String tableAlia) {
      super(clazz, tableName, tableAlia);
    }

    public static <T extends AbstractEntityPoJo<T, ?>> ConditionCustom<T> getInstance(
        final QueryColumn<T> queryColumn) {
      return new ConditionCustom<>(queryColumn);
    }

    public <V> ConditionCustom<T> notEq(@Nonnull final IConsumer<T, V> field, final V value) {
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
  }
}
