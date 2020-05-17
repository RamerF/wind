package io.github.ramerf.wind.demo.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.demo.entity.pojo.DemoProductPoJo.Type;
import io.github.ramerf.wind.demo.service.DemoProductService;
import io.swagger.annotations.Api;
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
@RequestMapping("/test")
@Api(tags = "测试专用")
@SuppressWarnings("all")
public class DemoProductController {
  @Resource private PrototypeBean prototypeBean;
  @Resource private DemoProductService service;

  @GetMapping(params = "type=1")
  public ResponseEntity<Rs<Object>> foo1() {
    QueryColumn<DemoProductPoJo> queryColumn =
        QueryColumnFactory.getInstance(DemoProductPoJo.class);
    Condition<DemoProductPoJo> condition =
        queryColumn.getCondition().gt(DemoProductPoJo::setId, 0L);
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
            .where(condition)
            .groupBy(clause.col(DemoProductPoJo::getName))
            .fetchAll(Ts.class);
    return Rs.ok();
  }

  @GetMapping(params = "type=2")
  public ResponseEntity<Rs<Object>> foo2() {
    QueryColumn<DemoProductPoJo> queryColumn =
        QueryColumnFactory.getInstance(DemoProductPoJo.class);
    Condition<DemoProductPoJo> condition =
        queryColumn.getCondition().gt(DemoProductPoJo::setId, 0L);
    final QueryEntityMetaData<DemoProductPoJo> queryEntityMetaData =
        queryColumn.getQueryEntityMetaData();
    final GroupByClause<DemoProductPoJo> clause = queryEntityMetaData.getGroupByClause();
    final Long one =
        prototypeBean
            .query()
            .select(queryColumn.sum(DemoProductPoJo::getId, "big_decimal"))
            .where(
                condition
                    .eq(DemoProductPoJo::setName, "ramer")
                    .eq(DemoProductPoJo::setCode, "code")
                    .isNull(DemoProductPoJo::setName)
                    .isNotNull(DemoProductPoJo::setName))
            .groupBy(clause.col(DemoProductPoJo::getName))
            .fetchOne(Long.class);
    return Rs.ok(one);
  }

  @GetMapping(params = "type=3")
  public ResponseEntity<Rs<Object>> foo3() {
    return Rs.ok(service.lists(condition -> condition.eq(DemoProductPoJo::setType, Type.PHONE)));
  }

  @Data
  public static class Ts {
    private BigDecimal bigDecimal;
    private String name2;
  }
}
