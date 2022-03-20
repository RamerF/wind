package io.github.ramerf.wind.core.mysql;

import ch.qos.logback.classic.Level;
import com.alibaba.druid.pool.DruidDataSource;
import io.github.ramerf.wind.WindApplication;
import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Sort.Direction;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSource;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSourceHolder;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.mysql.Foo.Type;
import io.github.ramerf.wind.core.plugin.Interceptor;
import io.github.ramerf.wind.core.plugin.Invocation;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.util.LogUtil;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.LongStream;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
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
            .createTime(LocalDateTime.now())
            .build();
  }

  @BeforeEach
  public void beforeEach() {
    final WindApplication application = WindApplication.run("mysql.yml");
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
    CndsGroup<Foo> group = CndsGroup.of(Foo.class);
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
                        .type(Type.SPORT)
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
                        .type(Type.SPORT)
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
    assertEquals(service.delete(id), 1);
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

  /** 自定义拦截器. */
  public static class FooInterceptor implements Interceptor {
    @Override
    public boolean supports(final Class<?> clazz) {
      // return Foo.class.isAssignableFrom(clazz);
      return true;
    }

    @Override
    public Object intercept(final Invocation invocation) throws Throwable {
      log.info("intercept:[{}]", "---------Foo Interceptor---------");
      log.info("intercept:[{}]", invocation.getTarget());
      log.info("intercept:[{}]", invocation.getMethod());
      log.info("intercept:[{}]", invocation.getArgs());
      log.info("intercept:[{}]", invocation.getExecType());
      return invocation.proceed();
    }
  }

  public static void main(String[] args) {
    Configuration configuration = new Configuration();
    final JdbcEnvironment jdbcEnvironment =
        new JdbcEnvironment(new JdbcTransactionFactory(), getDynamicDataSource());
    configuration.setJdbcEnvironment(jdbcEnvironment);
    configuration.setDdlAuto(DdlAuto.UPDATE);
    configuration.setEntityPackage("io.github.ramerf.wind.core.mysql");
    configuration.setInterceptorPackage("io.github.ramerf.wind.core.mysql");
    final WindApplication windApplication = WindApplication.run(configuration);
    final Dao dao = windApplication.getDaoFactory().getDao(true);
    LogUtil.setLoggerLevel(SimpleJdbcExecutor.class, Level.TRACE);
    //
    DynamicDataSourceHolder.push("d2");
    Foo foo1 = new Foo();
    foo1.setName(1 + "-" + LocalDateTime.now());
    dao.create(foo1);
    DynamicDataSourceHolder.poll();

    DynamicDataSourceHolder.push("d1");
    Foo foo2 = new Foo();
    foo2.setName(2 + "-" + LocalDateTime.now());
    dao.create(foo2);
    DynamicDataSourceHolder.poll();

    log.info("main:[{}]", foo1.getId() + "-" + foo2.getId());
    if (System.currentTimeMillis() % 2 == 0) {
      dao.commit(true);
    } else {
      dao.rollback(true);
    }
    // TODO WARN 动态数据源,Executor需要可以切换事务Transaction,使用TranactionHolder
  }

  private static DataSource getDynamicDataSource() {
    final DynamicDataSource dynamicDataSource = new DynamicDataSource();
    dynamicDataSource.addDataSource("d1", getDataSource1());
    dynamicDataSource.addDataSource("d2", getDataSource2());
    dynamicDataSource.setPrimary("d1");
    dynamicDataSource.setStrict(true);
    return dynamicDataSource;
  }

  @Nonnull
  private static DruidDataSource getDataSource1() {
    DruidDataSource dataSource = new DruidDataSource();
    dataSource.setUrl(
        "jdbc:mysql:///wind?serverTimezone=GMT%2B8&useUnicode=true&characterEncoding=UTF-8&allowPublicKeyRetrieval=true&useSSL=false");
    dataSource.setUsername("root");
    dataSource.setPassword("root");
    return dataSource;
  }

  @Nonnull
  private static DruidDataSource getDataSource2() {
    DruidDataSource dataSource = new DruidDataSource();
    dataSource.setUrl(
        "jdbc:mysql:///wind_2?serverTimezone=GMT%2B8&useUnicode=true&characterEncoding=UTF-8&allowPublicKeyRetrieval=true&useSSL=false");
    dataSource.setUsername("root");
    dataSource.setPassword("root");
    return dataSource;
  }
}
