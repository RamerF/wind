package io.github.ramerf.wind.demo;

import ch.qos.logback.classic.*;
import io.github.ramerf.wind.WindApplication;
import io.github.ramerf.wind.core.condition.Cnd;
import io.github.ramerf.wind.core.condition.Fields;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.service.BaseService;
import io.github.ramerf.wind.core.service.ServiceFactory;
import io.github.ramerf.wind.demo.entity.Foo;
import io.github.ramerf.wind.demo.service.FooService;
import io.github.ramerf.wind.demo.service.FooServiceImpl;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.LoggerFactory;

/**
 * The type Demo application.
 *
 * @since 2022.06.18
 * @author ramer
 */
@Slf4j
public class DemoApplication {
  public static void main(String[] args) {
    setLoggerLevel(Logger.ROOT_LOGGER_NAME, Level.INFO);
    // 打印sql
    setLoggerLevel(SimpleJdbcExecutor.class, Level.DEBUG);
    final WindApplication application = WindApplication.run("application.yml");

    // 手动提交事务
    try (final DaoManager daoManager = DaoManager.newInstance(application.getDaoFactory())) {
      daoManager.startManagedDao();
      // 查询指定字段
      final Foo foo =
          daoManager.fetchOne(
              Cnd.of(Foo.class).eq(Foo::setId, 1L), Fields.of(Foo.class).include(Foo::getId));
      foo.setName(LocalDateTime.now() + "");
      // 更新指定字段
      daoManager.update(foo, Fields.of(Foo.class).include(Foo::getName));
      daoManager.commit();
      // 事务回滚
      // daoManager.rollback();
    }

    final Dao dao = application.getDaoFactory().getDao();
    BaseService<Foo, Long> baseService = ServiceFactory.getService(dao, Foo.class, Long.class);
    final Foo foo = baseService.getOne(1L);
    foo.setName(LocalDateTime.now() + "");
    baseService.update(foo);

    final FooService fooService = ServiceFactory.getService(new FooServiceImpl(dao));
    fooService.foo(LocalDateTime.now() + "");
  }

  private static void setLoggerLevel(Class<?> clazz, Level level) {
    setLoggerLevel(clazz.getName(), level);
  }

  private static void setLoggerLevel(String loggerPackage, Level level) {
    LoggerContext loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
    List<Logger> loggers = loggerContext.getLoggerList();
    List<Logger> packageLoggerList =
        loggers.stream()
            .filter(a -> a.getName().startsWith(loggerPackage))
            .collect(Collectors.toList());
    if (packageLoggerList.isEmpty()) {
      loggerContext.getLogger(loggerPackage).setLevel(level);
      return;
    }
    for (Logger logger : packageLoggerList) {
      logger.setLevel(level);
    }
    packageLoggerList.forEach(logger -> logger.setLevel(level));
  }
}
