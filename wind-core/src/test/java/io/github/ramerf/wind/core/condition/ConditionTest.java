package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
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
public class ConditionTest {
  Condition<AbstractEntityPoJo> condition;
  Condition<AbstractEntityPoJo> condition2;

  @Before
  public void before() {
    log.info(" ConditionsTest.before : [{}]", "before");
    condition = Condition.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "a"));
    condition2 = Condition.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "b"));
  }

  @After
  public void after() {
    log.info(" ConditionsTest.after : [{}]", "after");
  }

  public void resetConditions() {
    condition = Condition.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "a"));
    condition2 = Condition.of(QueryColumnFactory.getInstance(AbstractEntityPoJo.class, "b"));
  }

  @Test
  public void eq() {
    resetConditions();
    Assert.assertEquals(
        "单值不加引号", "a.id=1", condition.eq(AbstractEntityPoJo::getId, 1L).getString());
    resetConditions();
    Assert.assertEquals(
        "单值加引号", "a.id='1'", condition.eq(AbstractEntityPoJo::getId, "1").getString());
  }

  @Test
  public void testEq() {
    resetConditions();
    Assert.assertEquals(
        "不添加条件", "", condition.eq(false, AbstractEntityPoJo::getId, 1L).getString());
  }

  @Test
  public void testEq1() {
    resetConditions();
    Assert.assertEquals(
        "连表eq",
        "a.id=b.id",
        condition
            .eq(AbstractEntityPoJo::getId, condition2, AbstractEntityPoJo::getId)
            .getString());
  }

  @Test
  public void testEq2() {}

  @Test
  public void ne() {
    resetConditions();
    Assert.assertEquals(
        "不添加条件", "a.id!=1", condition.ne(AbstractEntityPoJo::getId, 1L).getString());
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
        condition
            .eq(true, AbstractEntityPoJo::getIsDelete, false)
            .eq(true, AbstractEntityPoJo::getId, 1L)
            .or(condition2.eq(true, AbstractEntityPoJo::getIsDelete, true))
            .getString());
  }

  @Test
  public void toSqlVal() {
    resetConditions();
    Assert.assertEquals(
        "单值不加引号", "a.id=1", condition.eq(AbstractEntityPoJo::getId, 1L).getString());
    resetConditions();
    Assert.assertEquals(
        "单值加引号", "a.id='1'", condition.eq(AbstractEntityPoJo::getId, "1").getString());
    resetConditions();
    Assert.assertEquals(
        "多值不加引号",
        "a.id IN (1,2,3)",
        condition.in(AbstractEntityPoJo::getId, Arrays.asList(1, 2, 3)).getString());
    resetConditions();
    Assert.assertEquals(
        "多值加引号",
        "a.id IN ('1','2','3')",
        condition.in(AbstractEntityPoJo::getId, Arrays.asList("1", "2", "3")).getString());
  }
}
