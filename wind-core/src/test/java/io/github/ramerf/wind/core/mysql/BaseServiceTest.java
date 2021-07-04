package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.mysql.Foo.Type;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.util.StringUtils;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.annotation.Order;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Service层测试.
 *
 * @author ramer
 * @since 2020.8.29
 */
@Slf4j
@Sql("classpath:db-mysql.sql")
@ExtendWith(SpringExtension.class)
@ActiveProfiles("mysql")
@SpringBootTest(classes = MysqlApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Mysql 测试")
public class BaseServiceTest {
  private GenericService<Foo, Long> service;
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
            .column("non_match_column")
            .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
            // .bigText("")
            .isNumber(false)
            .isNull(false)
            .string(true)
            .nonNull(true)
            .typeJson(Type.SPORT)
            .typesJson(Arrays.asList(Type.PHONE, Type.SPORT))
            .build();
  }

  @BeforeEach
  public void before() {
    foo.setId(id);
    service = GenericService.with(Foo.class, Long.class);
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
  @DisplayName("查询单个:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne4() {
    service.getOne(
        query ->
            query.col(
                "(case name when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) as name,id"),
        condition -> condition.eq(Foo::setId, 10000L).and("name is not null"));
  }

  @Test
  @DisplayName("查询单个:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne5() {
    assertNotNull(service.getOne("select id,name from foo limit 1", IdNameResponse.class));
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
    assertNotNull(service.list(condition -> condition.gt(Foo::setId, 0L), 1, 10));
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
  @DisplayName("查询列表:自定义sql")
  @Transactional(rollbackFor = Exception.class)
  public List<Foo> testListAll3() {
    return service.listAll("select * from foo", Foo.class);
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
  @DisplayName("查询分页:带条件指定列,返回任意对象")
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
  public void testCreate1() {
    foo.setId(null);
    assertNotNull(service.create(foo));
  }

  @Test
  @Order(2)
  @DisplayName("单个创建: 指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate2() {
    foo.setId(null);
    // assertTrue(service.create(foo, fields -> fields.exclude(Foo::getAge)) > 0);
    assertNotNull(
        service.create(foo, fields -> fields.include(Foo::getAge, Foo::isString, Foo::isNumber)));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:域对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreate3() {
    foo.setId(null);
    assertNotNull(
        foo.create(
            fields ->
                fields
                    // 根据条件动态更新字段
                    .include(StringUtils.nonEmpty(foo.getName()), Foo::getName)
                    .include(Foo::getAge, Foo::isString, Foo::isNumber)));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet1() {
    foo.setId(null);
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @Order(1)
  @DisplayName("单个创建:返回对象,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateAndGet2() {
    foo.setId(null);
    assertNotNull(service.createAndGet(foo, fields -> fields.exclude(Foo::getBigText)));
  }

  @Test
  @DisplayName("批量创建")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch1() {
    foo.setId(null);
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
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(service.createBatch(list).isPresent());
  }

  @Test
  @DisplayName("批量创建:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testCreateBatch2() {
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
                        .column("non_match_column")
                        .build())
            .collect(toList());
    long start = System.currentTimeMillis();
    assertFalse(
        service
            .createBatch(
                list,
                fields -> fields.include(Foo::getName, Foo::getAge, Foo::isString, Foo::isNumber))
            .isPresent());
  }

  @Test
  @DisplayName("单个更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate1() {
    assertEquals(service.update(foo), 1);
  }

  @Test
  @DisplayName("单个更新:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate2() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertEquals(service.update(foo, fields -> fields.include(Foo::getName)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate3() {
    assertEquals(service.updateByCondition(foo, condition -> condition.eq(Foo::setId, id)), 1);
  }

  @Test
  @DisplayName("单个更新:条件更新,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate4() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertEquals(
        service.update(
            foo, //
            fields -> fields.include(Foo::getName),
            condition -> condition.eq(Foo::setId, id)),
        1);
  }

  @Test
  @DisplayName("单个更新:返回对象")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet1() {
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @DisplayName("单个更新:返回对象,指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateAndGet2() {
    foo.setName("<" + LocalDateTime.now() + ">");
    assertNotNull(service.updateAndGet(foo, fields -> fields.include(Foo::getName)));
  }

  @Test
  @DisplayName("批量更新")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch1() {
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
                        .column("non_match_column")
                        .build())
            .collect(toList());
    assertFalse(service.updateBatch(list).isPresent());
  }

  @Test
  @DisplayName("批量更新:指定属性")
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateBatch2() {
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
                        .column("non_match_column")
                        .build())
            .collect(toList());
    assertFalse(service.updateBatch(list, fields -> fields.include(Foo::getName)).isPresent());
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

  /**
   * @author ramer
   * @since 2020/8/5
   */
  @Getter
  @Setter
  public static class IdNameResponse {
    private Long id;
    private String name;
  }
}
