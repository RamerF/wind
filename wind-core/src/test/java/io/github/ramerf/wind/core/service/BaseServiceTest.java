package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.TestApplication;
import io.github.ramerf.wind.core.condition.SortColumn;
import io.github.ramerf.wind.core.condition.SortColumn.Order;
import io.github.ramerf.wind.core.entity.pojo.Foo;
import io.github.ramerf.wind.core.entity.pojo.Foo.Type;
import io.github.ramerf.wind.core.entity.response.IdNameResponse;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;
import static java.util.stream.Collectors.toList;

/**
 * service层测试.
 *
 * @author ramer
 * @since 05/08/2020
 */
@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes = TestApplication.class)
public class BaseServiceTest {
  @Resource private FooService service;

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void createAndGet() {
    log.info(
        "createAndGet:[{}]",
        service.createAndGet(
            Foo.builder()
                .name("demo")
                .textString("text")
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
                .build()));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void updateAndGet() {
    log.info(
        "updateAndGet:[{}]",
        service.updateAndGet(
            Foo.builder()
                .id(1L)
                .name("demo")
                .textString("text")
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
                .build(),
            Foo::getName,
            Foo::getStringList));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void count() {
    log.info("count:[{}]", service.count());
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testCount() {
    log.info("testCount:[{}]", service.count(condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testCount1() {
    log.info(
        "testCount1:[{}]",
        service.count(query -> query.col(Foo::getId), condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void getById() {
    log.info("getById:[{}]", service.getById(1L));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void getOne() {
    log.info("getOne:[{}]", service.getOne(condition -> condition.eq(Foo::setId, 1L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne() {
    log.info(
        "testGetOne:[{}]",
        service.getOne(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne1() {
    log.info(
        "testGetOne1:[{}]",
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testGetOne2() {
    log.info(
        "testGetOne2:[{}]",
        service.getOne(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void listByIds() {
    log.info("listByIds:[{}]", service.listByIds(Arrays.asList(1L, 2L, 3L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void list() {
    log.info("list:[{}]", service.list(condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList() {
    log.info(
        "testList:[{}]",
        service.list(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList1() {
    log.info(
        "testList1:[{}]",
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList2() {
    log.info(
        "testList2:[{}]",
        service.list(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList3() {
    log.info(
        "testList3:[{}]",
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testList4() {
    log.info(
        "testList4:[{}]",
        service.list(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void listAll() {
    log.info("listAll:[{}]", service.listAll(query -> query.col(Foo::getId).col(Foo::getName)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testListAll() {
    log.info(
        "testListAll:[{}]",
        service.listAll(query -> query.col(Foo::getId).col(Foo::getName), IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void page() {
    log.info(
        "page:[{}]",
        service.page(
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage() {
    log.info(
        "testPage:[{}]",
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage1() {
    log.info(
        "testPage1:[{}]",
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC)));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testPage2() {
    log.info(
        "testPage2:[{}]",
        service.page(
            query -> query.col(Foo::getId).col(Foo::getName),
            condition -> condition.gt(Foo::setId, 0L),
            1,
            10,
            SortColumn.by(Foo::getName, Order.DESC),
            IdNameResponse.class));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void create() {
    log.info(
        "create:[{}]",
        service.create(
            Foo.builder()
                .name("demo")
                .textString("text")
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
                .build()));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void createBatch() {
    final List<Foo> list =
        LongStream.range(0, 100)
            .mapToObj(
                i ->
                    Foo.builder()
                        // .id(1234123L)
                        .name("demo" + i)
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
    final int batch = service.createBatch(list, Foo::getName, Foo::getStringList);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void update() {
    log.info(
        "update:[{}]",
        service
            .update(
                Foo.builder()
                    .id(1L)
                    .name("demo")
                    .textString("text")
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
                    .build(),
                Foo::getName,
                Foo::getStringList)
            .orElse(0));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void testUpdate() {
    log.info(
        "testUpdate:[{}]",
        service.update(
            condition -> condition.eq(Foo::setId, 1L).isNotNull(Foo::setCreateTime),
            Foo.builder()
                .name("demo")
                .textString("text")
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
                .build(),
            Foo::getName,
            Foo::getStringList));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  public void updateBatch() {
    final List<Foo> list =
        LongStream.range(0, 100)
            .mapToObj(
                i ->
                    Foo.builder()
                        .id(i)
                        .name("demo" + i)
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
    service
        .updateBatch(list, Foo::getName, Foo::getStringList)
        // 只有当list不为空且更新记录数和list的大小不同时,才会执行下方的代码,入参为实际受影响的行数
        .ifPresent(affectRow -> log.info("updateBatch:affectRow[{}]", affectRow));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void delete() {
    log.info("delete:[{}]", service.delete(1L));
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void testDelete() {
    final long affectRow = service.delete(condition -> condition.eq(Foo::setId, 1L));
    log.info("testDelete:[{}]", affectRow);
  }

  @Test
  @Transactional(rollbackFor = Exception.class)
  public void deleteByIds() {
    service
        .deleteByIds(Arrays.asList(1L, 2L, 3L, 4L))
        // 只有当ids不为空且删除记录数和ids的大小不同时,才会执行下方的代码,入参为实际受影响的行数
        .ifPresent(affectRow -> log.info("deleteByIds:[{}]", affectRow));
  }
}
