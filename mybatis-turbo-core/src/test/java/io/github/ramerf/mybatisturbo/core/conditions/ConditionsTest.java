package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.mybatisturbo.core.factory.QueryColumnFactory;
import java.util.Arrays;
import lombok.extern.slf4j.Slf4j;
import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2019/12/31
 */
@Slf4j
@RunWith(JUnit4.class)
public class ConditionsTest {
  Conditions<AbstractEntityPoJo> conditions;
  Conditions<AbstractEntityPoJo> conditions2;

  @Before
  public void before() {
    log.info(" ConditionsTest.before : [{}]", "before");
    conditions = Conditions.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "a"));
    conditions2 = Conditions.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "b"));
  }

  @After
  public void after() {
    log.info(" ConditionsTest.after : [{}]", "after");
  }

  public void resetConditions() {
    conditions = Conditions.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "a"));
    conditions2 = Conditions.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "b"));
  }

  @Test
  public void eq() {
    resetConditions();
    Assert.assertEquals(
        "单值不加引号", "a.id=1", conditions.eq(AbstractEntityPoJo::getId, 1L).getCondition());
    resetConditions();
    Assert.assertEquals(
        "单值加引号", "a.id='1'", conditions.eq(AbstractEntityPoJo::getId, "1").getCondition());
  }

  @Test
  public void testEq() {
    resetConditions();
    Assert.assertEquals(
        "不添加条件", "", conditions.eq(false, AbstractEntityPoJo::getId, 1L).getCondition());
  }

  @Test
  public void testEq1() {
    resetConditions();
    Assert.assertEquals(
        "连表eq",
        "a.id=b.id",
        conditions
            .eq(AbstractEntityPoJo::getId, conditions2, AbstractEntityPoJo::getId)
            .getCondition());
  }

  @Test
  public void testEq2() {}

  @Test
  public void ne() {
    resetConditions();
    Assert.assertEquals(
        "不添加条件", "a.id!=1", conditions.ne(AbstractEntityPoJo::getId, 1L).getCondition());
  }

  @Test
  public void gt() {}

  @Test
  public void ge() {}

  @Test
  public void lt() {}

  @Test
  public void le() {}

  @Test
  public void like() {}

  @Test
  public void likeLeft() {}

  @Test
  public void likeRight() {}

  @Test
  public void notLike() {}

  @Test
  public void between() {}

  @Test
  public void notBetween() {}

  @Test
  public void isNull() {}

  @Test
  public void isNotNull() {}

  @Test
  public void exists() {}

  @Test
  public void in() {}

  @Test
  public void notIn() {}

  @Test
  public void or() {
    Assert.assertEquals(
        "单值不加引号",
        "a.is_delete=false AND a.id=1 OR (b.is_delete=true)",
        conditions
            .eq(true, AbstractEntityPoJo::getIsDelete, false)
            .eq(true, AbstractEntityPoJo::getId, 1L)
            .or(conditions2.eq(true, AbstractEntityPoJo::getIsDelete, true))
            .getCondition());
  }

  @Test
  public void toSqlVal() {
    resetConditions();
    Assert.assertEquals(
        "单值不加引号", "a.id=1", conditions.eq(AbstractEntityPoJo::getId, 1L).getCondition());
    resetConditions();
    Assert.assertEquals(
        "单值加引号", "a.id='1'", conditions.eq(AbstractEntityPoJo::getId, "1").getCondition());
    resetConditions();
    Assert.assertEquals(
        "多值不加引号",
        "a.id IN (1,2,3)",
        conditions.in(AbstractEntityPoJo::getId, Arrays.asList(1, 2, 3)).getCondition());
    resetConditions();
    Assert.assertEquals(
        "多值加引号",
        "a.id IN ('1','2','3')",
        conditions.in(AbstractEntityPoJo::getId, Arrays.asList("1", "2", "3")).getCondition());
  }
}
