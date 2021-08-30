package io.github.ramerf.wind.core.mysql;

import io.github.ramerf.wind.core.condition.*;
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
import org.springframework.data.domain.Sort.Direction;
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
  @DisplayName("Cnds条件构造工具类")
  @Transactional(rollbackFor = Exception.class)
  public void testCnds() {
    final Cnds<Foo> cnds =
        // 指定操作的表
        Cnds.of(Foo.class)
            // 条件
            .gt(Foo::setId, 0L)
            // 分页
            .limit(1, 10)
            // 分组
            .groupBy(Foo::getName)
            // 排序
            .orderBy(Foo::getId, Direction.DESC);
    log.info("testCnds:[{}]", cnds.getString());
  }

  @Test
  @DisplayName("统计")
  public void testCount() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).gt(Foo::setId, 0L);
    assertTrue(service.count(cnds) > 0);
  }

  @Test
  @DisplayName("查询单个:通过id查询")
  public void testGetById() {
    assertNotNull(service.getById(id));
  }

  @Test
  @DisplayName("查询单个:条件查询")
  public void testGetOne1() {
    assertNotNull(service.getOne(Cnds.of(Foo.class).eq(Foo::setId, id)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列")
  public void testGetOne2() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    final QueryColumn<Foo> queryColumn =
        QueryColumn.of(Foo.class).col(Foo::getName).col(Foo::getId);
    assertNotNull(service.getOne(cnds, queryColumn));
  }

  @Test
  @DisplayName("查询单个:条件查询排序")
  public void testGetOne3() {
    assertNotNull(service.getOne(Cnds.of(Foo.class).limit(1).orderBy(Foo::getId, Direction.DESC)));
  }

  @Test
  @DisplayName("查询单个:条件查询指定列,返回任意对象")
  public void testGetOne4() {
    assertNotNull(
        service.getOne(
            Cnds.of(Foo.class).eq(Foo::setId, id),
            QueryColumn.of(Foo.class).col(Foo::getId),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询单个:自定义sql")
  public void testGetOne5() {
    service.getOne(
        Cnds.of(Foo.class).eq(Foo::setId, id).and("name is not null"),
        QueryColumn.of(Foo.class)
            .col(
                "(case name when 'halo1' then '匹配1' when 'halo2' then '匹配2' else '未匹配' end) as name,id"));
  }

  @Test
  @DisplayName("查询单个:自定义sql")
  public void testGetOne6() {
    assertNotNull(service.fetchOneBySql("select id,name from foo limit 1", IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:通过id列表查询")
  public void testListByIds() {
    assertNotNull(service.listByIds(Arrays.asList(id, 2L, 3L)));
  }

  @Test
  @DisplayName("查询列表:条件查询")
  public void testList1() {
    assertNotNull(service.list(Cnds.of(Foo.class).gt(Foo::setId, 0L)));
  }

  @Test
  @DisplayName("查询列表:指定列,返回任意对象")
  public void testList2() {
    final Cnds<Foo> cnds = Cnds.of(Foo.class).eq(Foo::setId, id);
    final QueryColumn<Foo> queryColumn =
        QueryColumn.of(Foo.class).col(Foo::getName).col(Foo::getId);
    assertNotNull(service.list(cnds, queryColumn, IdNameResponse.class));
  }

  @Test
  @DisplayName("查询列表:条件查询指定页,带排序")
  public void testList3() {
    assertNotNull(
        service.list(
            Cnds.of(Foo.class)
                .gt(Foo::setId, 0L)
                .limit(1, 10)
                .orderBy(Foo::getId, Direction.DESC)));
  }

  @Test
  @DisplayName("查询列表:条件查询指定列,返回任意对象")
  public void testList5() {
    assertNotNull(
        service.list(
            Cnds.of(Foo.class).col(Foo::getId).col(Foo::getName).gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件,带排序")
  public void testPage1() {
    assertNotNull(
        service.page(
            Cnds.of(Foo.class).gt(Foo::setId, 0L).page(Pages.of(1, 10).desc(Foo::getName))));
  }

  @Test
  @DisplayName("查询分页:指定列,返回任意对象")
  public void testPage2() {
    assertNotNull(
        service.page(
            Cnds.of(Foo.class)
                .col(Foo::getId)
                .col(Foo::getName)
                .page(Pages.of(1, 10).desc(Foo::getName)),
            IdNameResponse.class));
  }

  @Test
  @DisplayName("查询分页:带条件指定列,返回任意对象")
  public void testPage3() {
    assertNotNull(
        service.page(
            Cnds.of(Foo.class)
                .col(Foo::getId)
                .col(Foo::getName)
                .gt(Foo::setId, 0L)
                .page(Pages.of(1, 10).desc(Foo::getName).desc(Foo::getId))));
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
