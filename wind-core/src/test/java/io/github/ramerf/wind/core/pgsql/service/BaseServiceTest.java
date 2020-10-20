package io.github.ramerf.wind.core.pgsql.service;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.pgsql.PgsqlApplication;
import io.github.ramerf.wind.core.pgsql.entity.pojo.Foo;
import io.github.ramerf.wind.core.pgsql.entity.pojo.Foo.Type;
import io.github.ramerf.wind.core.pgsql.entity.response.IdNameResponse;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Service层测试.
 *
 * @author Tang Xiaofeng
 * @since 2020.8.29
 */
@Slf4j
@Rollback
@Sql("classpath:db-pgsql.sql")
@ExtendWith(SpringExtension.class)
@ActiveProfiles("pgsql")
@SpringBootTest(classes = PgsqlApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Pgsql 测试")
public class BaseServiceTest {
  @Resource private FooService service;
  private static final Foo foo;
  private static final Long id = 10000L;

  static {
    foo =
        Foo.builder()
            .id(1L)
            .name("test")
            .textString("textString")
            .bigDecimal(BigDecimal.valueOf(100))
            .type(Type.SPORT)
            .intList(Arrays.asList(1, 3, 5))
            .intArr(new Integer[] {1, 4, 7})
            .longList(Arrays.asList(2L, 4L, 6L))
            .longArr(new Long[] {1L, 3L, 5L})
            // .stringList(Arrays.asList("3", "a", "6", "b"))
            .stringArr(new String[] {"2", "a", "b"})
            .column("non_match_column")
            .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
            .build();
  }

  @BeforeEach
  public void before() {
    foo.setId(id);
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:创建并返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet() {
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:创建并返回对象,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGetWithNull() {
    assertNotNull(service.createAndGetWithNull(foo, Collections.singletonList(Foo::getStringList)));
  }

  @Test
  @DisplayName("单个更新:更新并返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet() {
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @DisplayName("单个更新:更新并返回对象,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGetWithNull() {
    assertNotNull(service.updateAndGetWithNull(foo, Collections.singletonList(Foo::getStringList)));
  }

  @Test
  @DisplayName("count所有")
  @Transactional(rollbackFor = Exception.class)
  public void testCount1() {
    assertTrue(service.count() > 0);
  }

  @Test
  @DisplayName("count带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testCount2() {
    assertTrue(service.count(condition -> condition.gt(Foo::setId, 0L)) > 0);
  }

  @Test
  @DisplayName("count指定列带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testCount3() {
    final long count =
        service.count(query -> query.col(Foo::getId), condition -> condition.gt(Foo::setId, 0L));
    assertTrue(count > 0);
  }

  @Test
  @DisplayName("查询单个:通过id查询")
  @Transactional(rollbackFor = Exception.class)
  public void testGetById() {
    assertNotNull(service.getById(id));
  }

  @Test
  @DisplayName("查询单个:条件查询")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne1() {
    assertNotNull(service.getOne(condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne2() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne3() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:通过id列表查询")
  @Transactional(rollbackFor = Exception.class)
  public void testListByIds() {
    assertNotNull(service.listByIds(Arrays.asList(id, 2L, 3L)));
  }

  @Test
  @DisplayName("查询列表:条件查询")
  @Transactional(rollbackFor = Exception.class)
  public void testList1() {
    assertNotNull(service.list(condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @DisplayName("查询列表:指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testList2() {
    assertNotNull(
        service.list(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testList3() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @DisplayName("查询列表:条件查询指定页,带排序")
  @Transactional(rollbackFor = Exception.class)
  public void testList4() {
    assertNotNull(
        service.list(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testList5() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列指定页,返回任意对象,带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testList6() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:查询所有,指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testListAll1() {
    assertNotNull(service.listAll(query -> query.col(Foo::getId).col(Foo::getName)));
  }

  @Test
  @DisplayName("查询列表:查询所有,指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testListAll2() {
    assertNotNull(
        service.listAll(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件,带排序")
  @Transactional(rollbackFor = Exception.class)
  public void testPage1() {
    assertNotNull(
        service.page(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询分页:指定列,返回任意对象")
  @Transactional(rollbackFor = Exception.class)
  public void testPage2() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件指定列")
  @Transactional(rollbackFor = Exception.class)
  public void testPage3() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @DisplayName("查询分页:带条件指定列,返回任意对象,多个字段排序")
  @Transactional(rollbackFor = Exception.class)
  public void testPage4() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getId, SortColumn.Order.DESC).desc(Foo::getName).asc(Foo::getType),
            IdNameResponse.class));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate() {
    assertTrue(service.create(foo) > 0);
  }

  @Test
  @Order(2)
  @DisplayName("单个创建:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateWithNull() {
    assertTrue(service.createWithNull(foo, Collections.singletonList(Foo::getStringList)) > 0);
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service
            .createBatchWithNull(list, Arrays.asList(Foo::getName, Foo::getStringList))
            .isPresent());
  }

  @Test
  @DisplayName("批量创建:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatchWithNull() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service
            .createBatchWithNull(list, Arrays.asList(Foo::getName, Foo::getStringList))
            .isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate() {
    assertEquals(service.update(foo), 1);
  }

  @Test
  @DisplayName("单个更新:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateWithNull() {
    assertEquals(service.updateWithNull(foo, Collections.singletonList(Foo::getStringList)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateCondition() {
    assertEquals(service.update(condition -> condition.eq(Foo::setId, id), foo), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新,指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateConditionWithNull() {
    assertEquals(
        service.updateWithNull(
            condition -> condition.eq(Foo::setId, id),
            foo,
            Arrays.asList(Foo::getName, Foo::getStringList)),
        1);
  }

  @Test
  @DisplayName("批量更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("test" + i * i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    assertFalse(service.updateBatch(list).isPresent());
  }

  @Test
  @DisplayName("批量更新:指定保存可能为null的列")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatchWithNull() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("test" + i * i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Type.SPORT)
                        .intList(Arrays.asList(1, 3, 5))
                        .intArr(new Integer[] {1, 4, 7})
                        .longList(Arrays.asList(2L, 4L, 6L))
                        .longArr(new Long[] {1L, 3L, 5L})
                        .stringList(Arrays.asList("3", "a", "6", "b"))
                        .stringArr(new String[] {"2", "a", "b"})
                        .column("non_match_column")
                        .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
                        .build())
            .collect(toList());
    assertFalse(
        service
            .updateBatchWithNull(list, Arrays.asList(Foo::getName, Foo::getStringList))
            .isPresent());
  }

  @Test
  @DisplayName("更新指定字段:带条件")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateField() {
    foo.setName(LocalDateTime.now().toString());
    assertEquals(
        service.updateField(
            foo, fields -> fields.include(Foo::getName), condition -> condition.eq(Foo::setId, id)),
        1);
  }

  @Test
  @Order(30)
  @DisplayName("单个删除:通过id删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete1() {
    assertEquals(service.delete(id), 1);
  }

  @Test
  @Order(31)
  @DisplayName("批量删除:条件删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDelete2() {
    assertEquals(service.delete(condition -> condition.eq(Foo::setId, id)), 1);
  }

  @Test
  @Order(32)
  @DisplayName("批量删除:通过id列表删除")
  @Transactional(rollbackFor = Exception.class)
  public void testDeleteByIds() {
    assertTrue(service.deleteByIds(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
  }

  @Test
  @DisplayName("单个查询:默认不查询指定字段(大字段)")
  @Transactional(rollbackFor = Exception.class)
  public void testDontFetch() {
    // 默认不查询
    assertNull(service.getOne(condition -> condition.eq(Foo::setId, id)).getLargeText());
    // 指定查询该字段
    assertNotNull(
        service
            .getOne(
                query -> query.col(Foo::getLargeText), condition -> condition.eq(Foo::setId, id))
            .getLargeText());
  }
}
