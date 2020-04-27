package io.github.ramerf.mybatisturbo.core.statement;

import io.github.ramerf.mybatisturbo.core.entity.pojo.AbstractEntityPoJo;
import java.util.List;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/13
 */
@Slf4j
@RunWith(JUnit4.class)
public class InsertStatementTest {
  //
  //  @Test
  //  public void insertTest() {
  //    final InsertValue insertValue =
  //        InsertStatement.getInstance()
  //            .insert(DemoProduct.class)
  //            .columns(
  //                AbstractEntityPoJo::getId,
  //                DemoProduct::getName,
  //                DemoProduct::getDemoProductCategoryId)
  //            .values(1L, "names", 11L);
  //    assertEquals("插入sql生成有误", "demo_product", insertValue.getInsertStr());
  //    assertEquals("插入sql生成有误", "(id,name,demo_product_category_id)", insertValue.getColumnStr());
  //    assertEquals("插入sql生成有误", "(1,'names',11)", insertValue.getValuesStr());
  //  }
  //
  //  @Test
  //  public void insertBatchTest() {
  //    final InsertValue insertValue =
  //        InsertStatement.getInstance()
  //            .insert(DemoProduct.class)
  //            .columns(
  //                AbstractEntityPoJo::getId,
  //                DemoProduct::getName,
  //                DemoProduct::getDemoProductCategoryId)
  //            .values(1L, "names", 11L)
  //            .values(2L, "names2", 22L)
  //            .values(3L, "names3", 33L);
  //    assertEquals("插入sql批量生成有误", "demo_product", insertValue.getInsertStr());
  //    assertEquals("插入sql批量生成有误", "(id,name,demo_product_category_id)",
  // insertValue.getColumnStr());
  //    assertEquals(
  //        "插入sql批量生成有误",
  //        "(1,'names',11),(2,'names2',22),(3,'names3',33)",
  //        insertValue.getValuesStr());
  //  }
  //
  //  /** 测试复合值. */
  //  @Test
  //  public void insertComplexTest() {
  //    List<Long> skuIds = new ArrayList<>();
  //    skuIds.add(1L);
  //    skuIds.add(2L);
  //    skuIds.add(3L);
  //    List<String> texts = new ArrayList<>();
  //    texts.add("text1");
  //    texts.add("text2");
  //    texts.add("text3");
  //    final InsertValue insertValue =
  //        InsertStatement.getInstance()
  //            .insert(DemoProduct.class)
  //            .columns(AbstractEntityPoJo::getId, DemoProduct::getSkuIds, DemoProduct::getTexts)
  //            .values(2L, skuIds, texts);
  //    assertEquals("插入sql复合生成有误", "demo_product", insertValue.getInsertStr());
  //    assertEquals("插入sql复合生成有误", "(id,sku_ids,texts)", insertValue.getColumnStr());
  //    assertEquals("插入sql复合生成有误", "(2,'{1,2,3}','{text1,text2,text3}')",
  // insertValue.getValuesStr());
  //  }

  @Getter
  //  @TableName("demo_product")
  @SuppressWarnings("unused")
  public static class DemoProduct extends AbstractEntityPoJo {
    private String name;

    private Long demoProductCategoryId;

    private List<Long> skuIds;

    private String[] texts;
  }
}
