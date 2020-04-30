package io.github.ramerf.mybatisturbo.test.controller;

import io.github.ramerf.mybatisturbo.core.conditions.*;
import io.github.ramerf.mybatisturbo.core.config.PrototypeBean;
import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.entity.response.Rs;
import io.github.ramerf.mybatisturbo.core.factory.QueryColumnFactory;
import io.github.ramerf.mybatisturbo.test.entity.pojo.DemoProductPoJo;
import io.github.ramerf.mybatisturbo.test.service.DemoProductService;
import io.swagger.annotations.Api;
import java.math.BigDecimal;
import java.util.*;
import javax.annotation.Resource;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020/4/28
 */
@Slf4j
@RestController
@RequestMapping("/test")
@Api(tags = "测试专用")
public class DemoProductController {
  @Resource private PrototypeBean prototypeBean;
  @Resource private DemoProductService service;

  @GetMapping
  public ResponseEntity<Rs<Object>> foo() {
    final QueryColumn<DemoProductPoJo> queryColumn =
        QueryColumnFactory.getInstance(DemoProductPoJo.class);
    final Conditions<DemoProductPoJo> conditions =
        queryColumn.getConditions().gt(DemoProductPoJo::getId, 0L);
    final QueryEntityMetaData<DemoProductPoJo> queryEntityMetaData =
        queryColumn.getQueryEntityMetaData();
    final GroupByClause<DemoProductPoJo> clause = queryEntityMetaData.getGroupByClause();
    //    clause.
    final List<Ts> one =
        prototypeBean
            .query()
            .select(
                queryColumn
                    .sum(DemoProductPoJo::getId, "big_decimal")
                    .col(DemoProductPoJo::getName, "name2"))
            .where(conditions)
            .groupBy(clause.col(DemoProductPoJo::getName))
            .fetch(Ts.class);
    //    final BigDecimal one =
    //        service.getOne(
    //            query -> query.sum(DemoProductPoJo::getBigDecimal),
    //            condition -> condition.gt(DemoProductPoJo::getId, 0L),
    //            BigDecimal.class);
    //    log.info("foo:[{}]", one);
    return Rs.ok(
        service.list(
            query -> query.col(DemoProductPoJo::getName),
            condition -> condition.lt(AbstractEntityPoJo::getCreateTime, new Date()),
            String.class));
  }

  @Data
  public static class Ts {
    private BigDecimal bigDecimal;
    private String name2;
  }
}
