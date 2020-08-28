package io.github.ramerf.wind.core.mysql.service;

import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.mysql.MysqlApplication;
import io.github.ramerf.wind.core.mysql.entity.pojo.Foo;
import io.github.ramerf.wind.core.mysql.entity.pojo.Foo.Type;
import io.github.ramerf.wind.core.mysql.entity.response.IdNameResponse;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * service层测试.
 *
 * @author ramer
 * @since 05/08/2020
 */
@Slf4j
@Sql("classpath:db-mysql.sql")
@ExtendWith(SpringExtension.class)
@ActiveProfiles("mysql")
@SpringBootTest(classes = MysqlApplication.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Mysql 测试")
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
  @Transactional(rollbackFor = Exception.class)
  public void createAndGet() {
    assertNotNull(service.createAndGet(foo));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void updateAndGet() {
    assertNotNull(service.updateAndGet(foo));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void count() {
    assertTrue(service.count() > 0);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testCount() {
    assertTrue(service.count(condition -> condition.gt(Foo::setId, 0L)) > 0);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testCount1() {
    final long count =
        service.count(query -> query.col(Foo::getId), condition -> condition.gt(Foo::setId, 0L));
    assertTrue(count > 0);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void getById() {
    assertNotNull(service.getById(id));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void getOne() {
    assertNotNull(service.getOne(condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne1() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne2() {
    assertNotNull(
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.eq(Foo::setId, id),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void listByIds() {
    assertNotNull(service.listByIds(Arrays.asList(id, 2L, 3L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void list() {
    assertNotNull(service.list(condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList() {
    assertNotNull(
        service.list(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList1() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList2() {
    assertNotNull(
        service.list(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList3() {
    assertNotNull(
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList4() {
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
  @Transactional(rollbackFor = Exception.class)
  public void listAll() {
    assertNotNull(service.listAll(query -> query.col(Foo::getId).col(Foo::getName)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testListAll() {
    assertNotNull(
        service.listAll(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void page() {
    assertNotNull(
        service.page(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage1() {
    assertNotNull(
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, SortColumn.Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage2() {
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
  @Transactional(rollbackFor = Exception.class)
  public void create() {
    assertTrue(service.create(foo) > 0);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void createBatch() {
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
        service.createBatchWithNull(list, Collections.singletonList(Foo::getName)).isPresent());
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testUpdate() {
    assertEquals(service.update(foo), 1);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testUpdateCondition() {
    assertEquals(
        service.updateWithNull(
            condition -> condition.eq(Foo::setId, id),
            foo,
            Collections.singletonList(Foo::getName)),
        1);
  }

  @Test
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
                        .column("non_match_column")
                        .build())
            .collect(toList());
    assertFalse(
        service.updateBatchWithNull(list, Collections.singletonList(Foo::getName)).isPresent());
  }

  @Test
  @Order(30)
  @Transactional(rollbackFor = Exception.class)
  public void testDelete() {
    assertEquals(service.delete(id), 1);
  }

  @Test
  @Order(31)
  @Transactional(rollbackFor = Exception.class)
  public void testDelete2() {
    assertEquals(service.delete(condition -> condition.eq(Foo::setId, id)), 1);
  }

  @Test
  @Order(32)
  @Transactional(rollbackFor = Exception.class)
  public void testDeleteByIds() {
    assertTrue(service.deleteByIds(Arrays.asList(id, 2L, 3L, 4L)).orElse(0) > 0);
  }
}
