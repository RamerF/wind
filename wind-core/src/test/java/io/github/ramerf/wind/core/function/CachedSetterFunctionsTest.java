package io.github.ramerf.wind.core.function;

import java.lang.reflect.Field;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

/**
 * @author ramer
 * @since 01/01/2022
 */
@Slf4j
public class CachedSetterFunctionsTest {
  @Test
  public void test() throws NoSuchFieldException {
    Foo foo = new Foo();
    SetterFunction<Foo, String> name = Foo::setName;
    if (System.currentTimeMillis() % 2 == 0) {
      CachedSetterFunctions.put(name);
    }
    Field field = Foo.class.getDeclaredField("name");
    if (CachedSetterFunctions.invoke(field, foo, "ramer")) {
      log.info("{}", "缓存调用");
    } else {
      log.info("{}", "方法未缓存");
    }
    log.info("name={}", foo.getName());
  }

  @Data
  public static class Foo {
    private Long id;
    private String name;
  }
}
