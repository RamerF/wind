package io.github.ramerf.wind.test.controller;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.PrototypeBean;
import io.github.ramerf.wind.core.entity.response.Rs;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.test.entity.pojo.DemoProductPoJo;
import io.github.ramerf.wind.test.entity.pojo.DemoProductPoJo.Type;
import io.github.ramerf.wind.test.service.DemoProductService;
import io.swagger.annotations.Api;
import java.math.BigDecimal;
import java.sql.Types;
import java.util.BitSet;
import java.util.List;
import javax.annotation.Resource;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.*;
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
  @Resource private JdbcTemplate jdbcTemplate;

  @GetMapping(params = "type=1")
  public ResponseEntity<Rs<Object>> foo1() {
    QueryColumn<DemoProductPoJo> queryColumn =
        QueryColumnFactory.getInstance(DemoProductPoJo.class);
    Condition<DemoProductPoJo> condition =
        queryColumn.getCondition().gt(DemoProductPoJo::getId, 0L);
    final QueryEntityMetaData<DemoProductPoJo> queryEntityMetaData =
        queryColumn.getQueryEntityMetaData();
    final GroupByClause<DemoProductPoJo> clause = queryEntityMetaData.getGroupByClause();
    //    clause.
    final long start = System.currentTimeMillis();
    final List<Ts> one =
        prototypeBean
            .query()
            .select(
                queryColumn
                    .sum(DemoProductPoJo::getId, "big_decimal")
                    .col(DemoProductPoJo::getName, "name2"))
            .where(condition)
            .groupBy(clause.col(DemoProductPoJo::getName))
            .fetch(Ts.class);

    queryColumn = QueryColumnFactory.getInstance(DemoProductPoJo.class);
    condition = queryColumn.getCondition().gt(DemoProductPoJo::getId, 0L);
    for (int i = 0; i < 10; i++) {
      prototypeBean.query().select(queryColumn).where(condition).fetch(Ts.class);
    }
    log.info("foo:[{}ms]", (System.currentTimeMillis() - start));
    return Rs.ok();
  }

  @GetMapping(params = "type=2")
  public ResponseEntity<Rs<Object>> foo2() {
    final BitSet set = BitSet.valueOf(new byte[] {0x1, 0x0});
    final SqlParameterValue value = new SqlParameterValue(Types.BLOB, set);
    jdbcTemplate.update("select * from demo_product where bit_set=?", value);
    return Rs.ok(
        jdbcTemplate.query(
            "select * from demo_product where is_delete=false",
            new BeanPropertyRowMapper<>(Ts.class)));
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

  public static void main(String[] args) {}
}
