package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.exception.ReflectiveInvokeException;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@Slf4j
@DisplayName("BeanUtils 测试")
public class BeanUtilsTest {
  @Test
  @DisplayName("获取指定包下,指定接口/类的子类")
  public void testScanClasses() throws IOException {
    // TODO WARN 参考: org/apache/ibatis/type/TypeAliasRegistry.java:130
    BeanUtils.scanClasses("io.github.ramerf", ITypeHandler.class).forEach(System.out::println);
  }

  @Test
  @DisplayName("反射调用方法")
  public void testInvokeMethod() throws NoSuchMethodException {
    final Method method = String.class.getMethod("equals", Object.class);
    assertEquals(true, BeanUtils.invokeMethod("string", method, "string"), "调用方法");
    assertThrows(
        ReflectiveInvokeException.class,
        () -> BeanUtils.invokeMethod(null, method, "string"),
        "调用方法抛出异常");
  }

  @Test
  @DisplayName("获取私有字段")
  public void testRetrievePrivateFields() {
    final ArrayList<Field> fields = BeanUtils.retrieveDeclaredFields(Ts.class);
    log.info("[{}]", fields);
    final HashSet<Field> container = new HashSet<>();
    log.info("[{}]", BeanUtils.retrieveDeclaredFields(Ts.class, container));
  }

  @Test
  @DisplayName("获取公开方法")
  public void testGetDeclaredField() {
    log.info("[{}]", BeanUtils.getDeclaredField(Ts.class, "name"));
  }
}
