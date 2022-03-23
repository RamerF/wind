package io.github.ramerf.wind.core.pgsql;

import io.github.ramerf.wind.WindApplication;
import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.executor.DaoFactory;
import io.github.ramerf.wind.core.pgsql.Foo.Type;
import io.github.ramerf.wind.core.service.GenericService;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Service层测试.
 *
 * @author ramer
 * @since 2020.8.29
 */
@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Pgsql 测试")
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
            .intList(Arrays.asList(1, 3, 5))
            .intArr(new Integer[] {1, 4, 7})
            .longList(Arrays.asList(2L, 4L, 6L))
            .longArr(new Long[] {1L, 3L, 5L})
            .stringList(Arrays.asList("3", "a", "6", "b"))
            .stringArr(new String[] {"2", "a", "b"})
            .column("non_match_column")
            .bitSet(BitSet.valueOf(new byte[] {0x11, 0x0, 0x1, 0x1, 0x0}))
            .typeJson(Type.SPORT)
            .typesJson(Arrays.asList(Type.PHONE, Type.SPORT))
            .build();
  }

  @BeforeEach
  public void beforeEach() {
    final WindApplication application = WindApplication.run("pgsql.yml");
    final Dao dao = DaoFactory.of(application.getConfiguration()).getDao();
    foo.setId(id);
    service = GenericService.with(dao, Foo.class, Long.class);
    if (service.getOne(foo.getId()) == null) {
      service.create(foo);
    }
  }

  @AfterEach
  public void afterEach() {
    service.update("delete from foo where id=" + id, ps -> {});
  }

  @Test
  @Order(10)
  @DisplayName("条件构造工具类")
  public void testCnds() {
    final Cnd<Foo> cnds =
        // 指定操作的表
        Cnd.of(Foo.class)
            // 条件
            .gt(Foo::setId, 0L)
            // 自定义sql
            .and("name is not null")
            // 分页
            .limit(1, 10)
            // 分组
            .groupBy(Foo::getName)
            // 排序
            .orderBy(Foo::getCreateTime)
            .orderBy(Foo::getId, Direction.ASC);
    // 条件组
    CndGroup<Foo> group = CndGroup.of(Foo.class);
    group.orLike(Foo::setName, "name").orLike(Foo::setTextString, "name");
    cnds.and(group);

    final Fields<Foo> fields =
        Fields.of(Foo.class)
            // 查询指定列
            .include(Foo::getId)
            .include(Foo::getName)
            .exclude(Foo::getType)
            .exclude(Foo::getUpdateTime);
    final String includes =
        fields.getIncludeFields().stream().map(Field::getName).collect(joining(","));
    assertEquals("id,name", includes, "Cnds includes");
    final String excludes =
        fields.getExcludeFields().stream().map(Field::getName).collect(joining(","));
    assertEquals("type,updateTime", excludes, "Cnds excludes");
  }

  @Test
  @Order(5)
  @DisplayName("统计")
  public void testCount() {
    final Cnd<Foo> cnds = Cnd.of(Foo.class).gt(Foo::setId, 0L);
    assertTrue(service.count(cnds) > 0);
  }

  @Test
  @Order(6)
  @DisplayName("查询单个")
  public void testGetOne() {
    // 通过id查询
    assertNotNull(service.getOne(id));
    // 条件查询
    assertNotNull(service.getOne(Cnd.of(Foo.class).eq(Foo::setId, id)));
    // 条件查询指定列
    final Cnd<Foo> cnds = Cnd.of(Foo.class).eq(Foo::setId, id);
    final Fields<Foo> fields = Fields.of(Foo.class).include(Foo::getName).include(Foo::getId);
    assertNotNull(service.getOne(cnds, fields));
    // 指定返回对象
    assertNotNull(
        service.getOne(
            Cnd.of(Foo.class).eq(Foo::setId, id),
            Fields.of(Foo.class).include(Foo::getId),
            IdNameResponse.class));
    // 自定义sql
    assertNotNull(service.getOne("select id,name from foo limit 1", IdNameResponse.class));
  }

  @Test
  @Order(7)
  @DisplayName("查询列表")
  public void testList() {
    // 通过id列表查询
    assertNotNull(service.list(Arrays.asList(id, 2L, 3L)));
    final Cnd<Foo> cnds = Cnd.of(Foo.class).eq(Foo::setId, id);
    // 条件查询
    assertNotNull(service.list(cnds));
    // 查询指定列
    final Fields<Foo> fields = Fields.of(Foo.class).include(Foo::getId).include(Foo::getName);
    assertNotNull(service.list(cnds, fields, IdNameResponse.class));
    // 查询指定页
    cnds.limit(1, 10).orderBy(Foo::getId, Direction.DESC);
    assertNotNull(service.list(cnds));
    // 指定返回对象
    assertNotNull(service.list(cnds, fields, IdNameResponse.class));
  }

  @Test
  @Order(8)
  @DisplayName("查询分页")
  public void testPage() {
    final Cnd<Foo> cnds = Cnd.of(Foo.class).gt(Foo::setId, 0L).limit(1, 5).orderBy(Foo::getName);
    assertNotNull(service.page(cnds));
    // 指定列
    final Fields<Foo> fields = Fields.of(Foo.class).include(Foo::getId).include(Foo::getName);
    final Page<Foo> page = service.page(cnds, fields);
    final long count = service.count(cnds);
    assertEquals((int) Math.ceil((double) count / (double) 5), page.getTotalPages(), "总页数");
    assertEquals(1, page.getPageNumber(), "当前页码");
    assertEquals(5, page.getSize(), "每页大小");
    assertEquals(page.getTotalElements(), count, "总记录数");
    assertFalse(page.isLast(), "是否最后一页");
  }

  @Test
  @Order(1)
  @DisplayName("单个创建")
  public void testCreate() {
    foo.setId(null);
    service.create(foo);
    assertNotNull(foo.getId());
    // 保存指定字段
    final Fields<Foo> fields =
        Fields.of(Foo.class).include(Foo::getAge, Foo::isString, Foo::isNumber);
    // 排除指定字段
    // fields.exclude(Foo::getAge)
    service.create(foo, fields);
    assertNotNull(foo.getId());
  }

  @Test
  @Order(2)
  @DisplayName("批量创建")
  public void testCreateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .name("test" + i)
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Foo.Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    assertFalse(service.createBatch(list).isPresent());
    assertNotNull(list.get(0).getId());
  }

  @Test
  @Order(3)
  @DisplayName("单个更新")
  public void testUpdate() {
    foo.setName(LocalDateTime.now() + "");
    assertEquals(service.update(foo), 1);
    // 指定属性
    assertEquals(service.update(foo, Fields.of(Foo.class).include(Foo::getName)), 1);
    // 条件更新
    assertEquals(service.update(foo, Cnd.of(Foo.class).eq(Foo::setId, id)), 1);
    // 条件更新指定字段
    assertEquals(
        service.update(
            foo, //
            Fields.of(Foo.class).include(Foo::getName),
            Cnd.of(Foo.class).eq(Foo::setId, id)),
        1);
  }

  @Test
  @Order(4)
  @DisplayName("批量更新")
  public void testUpdateBatch() {
    final List<Foo> list =
        LongStream.range(1, 101)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name(LocalDateTime.now() + "")
                        .textString("text" + i)
                        .bigDecimal(BigDecimal.valueOf(100 + i))
                        .type(Foo.Type.SPORT)
                        .column("non_match_column")
                        .build())
            .collect(toList());
    // 可选指定更新字段
    assertFalse(service.updateBatch(list, Fields.of(Foo.class).include(Foo::getName)).isPresent());
  }

  @Test
  @Order(10)
  @DisplayName("删除")
  public void testDelete() {
    // 通过id删除
    assertEquals(1, service.delete(id));
    // 通过id列表删除
    assertTrue(service.delete(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
    // 条件删除
    assertEquals(service.delete(Cnd.of(Foo.class).eq(Foo::setId, id)), 1);
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
