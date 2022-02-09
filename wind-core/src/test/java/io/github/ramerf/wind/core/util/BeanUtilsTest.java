package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import java.io.IOException;
import java.util.ArrayList;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@Slf4j
@DisplayName("BeanUtils 测试")
public class BeanUtilsTest {
  @Test
  @DisplayName("获取指定包下,指定接口/类的子类")
  public void testScanClasses() throws IOException {
    BeanUtils.scanClasses("io.github.ramerf", ITypeHandler.class).forEach(System.out::println);
  }

  @Test
  public void testInvoke() {
    BeanUtils.invoke(null, String.class.getMethods()[0], "string");
    BeanUtils.invoke(null, String.class.getMethods()[0], "string")
        .ifPresent(e -> log.info("调用失败处理[{}]", e.getClass()));
  }

  @Test
  public void testRetrievePrivateFields() {
    log.info("[{}]", BeanUtils.retrievePrivateFields(Ts.class, ArrayList::new));
  }

  @Test
  public void testGetDeclaredField() {
    log.info("[{}]", BeanUtils.getDeclaredField(Ts.class, "name"));
  }
}
