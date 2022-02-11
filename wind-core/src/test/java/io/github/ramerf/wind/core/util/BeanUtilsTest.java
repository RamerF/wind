package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.exception.ReflectiveInvokeException;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionManager;
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
  @DisplayName("获取指定包下,指定接口/类的子类")
  public void testInvoke() throws NoSuchMethodException {
    final Method method = String.class.getMethod("equals", Object.class);
    assertEquals(true, BeanUtils.invokeMethod("string", method, "string"), "调用方法");
    assertThrows(
        ReflectiveInvokeException.class,
        () -> BeanUtils.invokeMethod(null, method, "string"),
        "调用方法抛出异常");
  }

  @Test
  public void testRetrievePrivateFields() {
    final ArrayList<Field> fields = BeanUtils.retrievePrivateFields(Ts.class);
    log.info("[{}]", fields);
    final HashSet<Field> container = new HashSet<>();
    log.info("[{}]", BeanUtils.retrievePrivateFields(Ts.class, container));
  }

  @Test
  public void testGetDeclaredField() throws Exception {
    log.info("[{}]", BeanUtils.getDeclaredField(Ts.class, "name"));
    final SqlSession sqlSession =
        SqlSessionManager.newInstance(new FileInputStream("")).openSession();
    sqlSession.getMapper(Object.class);
    sqlSession.commit();
  }
}
