package io.github.ramerf.wind.core.mysql;

import ch.qos.logback.classic.Level;
import com.alibaba.druid.pool.DruidDataSource;
import io.github.ramerf.wind.WindApplication;
import io.github.ramerf.wind.core.condition.Cnd;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.DdlAuto;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSource;
import io.github.ramerf.wind.core.jdbc.dynamicdatasource.DynamicDataSourceHolder;
import io.github.ramerf.wind.core.jdbc.transaction.jdbc.JdbcTransactionFactory;
import io.github.ramerf.wind.core.util.LogUtil;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import net.sf.cglib.core.DebuggingClassWriter;
import org.junit.jupiter.api.*;

@Slf4j
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@DisplayName("Dao Mysql 测试")
public class DaoTest {

  @BeforeAll
  public static void beforeEach() {
    LogUtil.setLoggerLevel(SimpleJdbcExecutor.class, Level.TRACE);
    System.setProperty(
        DebuggingClassWriter.DEBUG_LOCATION_PROPERTY, "C:\\users\\ramer\\Desktop\\wind");
  }

  @Test
  @DisplayName("单一数据源事务")
  public void dao1() {
    final DaoFactory daoFactory = getDaoFactory(getDataSource1());
    final Dao dao = daoFactory.getDao();
    Foo foo1 = new Foo();

    foo1.setName(1 + "-" + LocalDateTime.now());
    dao.create(foo1);
    if (System.currentTimeMillis() % 2 == 0) {
      dao.commit(true);
      log.info("dao1:[>>>>>>>>>>>>>>>>>commit>>>>>>>>>>>>>>>>>]");
    } else {
      dao.rollback(true);
      log.info("dao1:[>>>>>>>>>>>>>>>>>rollback>>>>>>>>>>>>>>>>>]");
    }
    Foo foo2 = new Foo();
    foo2.setCreateTime(LocalDateTime.now());
    foo2.setName(2 + "-" + LocalDateTime.now());
    dao.create(foo2);
    dao.commit(true);
    final List<Foo> foo = dao.fetchAll(Cnd.of(Foo.class));
    log.info("dao1:[{}]", foo1.getId() + "-" + foo2.getId());
  }

  @Test
  @DisplayName("单一数据源事务2")
  public void dao2() {
    final DaoFactory daoFactory = getDaoFactory(getDataSource3());
    final Dao dao = daoFactory.getDao();
    final long maxId =
        Optional.ofNullable(dao.fetchOne(Cnd.of(Foo.class).desc(Foo::getId).limit(1)))
            .map(Foo::getId)
            .orElse(0L);
    Foo foo1 = new Foo();
    foo1.setName(1 + "-" + LocalDateTime.now());
    dao.create(foo1);
    Foo foo2 = new Foo();
    foo2.setName(2 + "-" + LocalDateTime.now());
    dao.create(foo2);
    log.info("dao2:[{}]", foo1.getId() + "-" + foo2.getId());
    dao.rollback(true);
    Assertions.assertEquals(
        maxId,
        Optional.ofNullable(dao.fetchOne(Cnd.of(Foo.class).desc(Foo::getId).limit(1)))
            .map(Foo::getId)
            .orElse(0L));
  }

  @Test
  @DisplayName("动态数据源事务")
  public void dao3() {
    final DaoFactory daoFactory = getDaoFactory(getDynamicDataSource());
    final Dao dao = daoFactory.getDao();
    DynamicDataSourceHolder.push("d2");
    Foo foo1 = new Foo();
    foo1.setName(1 + "-" + LocalDateTime.now());
    dao.create(foo1);
    DynamicDataSourceHolder.poll();
    final Dao dao2 = daoFactory.getDao();
    DynamicDataSourceHolder.push("d1");
    Foo foo2 = new Foo();
    foo2.setName(2 + "-" + LocalDateTime.now());
    dao2.create(foo2);
    DynamicDataSourceHolder.poll();

    log.info("dao3:[{}]", foo1.getId() + "-" + foo2.getId());
    if (System.currentTimeMillis() % 2 == 0) {
      log.info("dao3:[>>>>>>>>>>>>>>>>>commit>>>>>>>>>>>>>>>>>]");
      dao.commit(true);
      dao2.commit(true);
    } else {
      log.info("dao3:[>>>>>>>>>>>>>>>>>rollback>>>>>>>>>>>>>>>>>]");
      dao.rollback(true);
      dao2.rollback(true);
    }
  }

  private static DaoFactory getDaoFactory(final DataSource dataSource) {
    Configuration configuration = new Configuration();
    configuration.setJdbcEnvironment(new JdbcEnvironment(new JdbcTransactionFactory(), dataSource));
    configuration.setDdlAuto(DdlAuto.UPDATE);
    configuration.setEntityPackage("io.github.ramerf.wind.core.mysql");
    configuration.setInterceptorPackage("io.github.ramerf.wind.core.mysql");
    final WindApplication windApplication = WindApplication.run(configuration);
    return windApplication.getDaoFactory();
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

  @Nonnull
  private static DruidDataSource getDataSource3() {
    DruidDataSource dataSource = new DruidDataSource();
    dataSource.setUrl("jdbc:sqlite:C:\\Users\\ramer\\Desktop\\gogs.db");
    dataSource.setUsername("root");
    dataSource.setPassword("root");
    return dataSource;
  }
}
