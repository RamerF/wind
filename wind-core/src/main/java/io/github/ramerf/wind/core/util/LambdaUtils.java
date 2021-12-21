package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.TestLambda;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.*;
import java.lang.invoke.SerializedLambda;
import java.lang.ref.WeakReference;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Lambda utils.
 *
 * @author ramer
 * @since 2019 /12/26
 */
@Slf4j
public final class LambdaUtils {
  /** SerializedLambda 反序列化缓存 */
  private static final Map<Class<?>, WeakReference<SerializedLambda>> LAMBDA_MAP =
      new ConcurrentHashMap<>();

  /**
   * 获取lambda表达式对应的方法名.
   *
   * @param fieldFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(FieldFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getMethodName(FieldFunction fieldFunction) {
    return serializedLambda(fieldFunction).getImplMethodName();
  }

  /**
   * 获取lambda表达式对应的方法引用类名全路径.
   *
   * @param fieldFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(FieldFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getImplClassFullPath(FieldFunction fieldFunction) {
    return serializedLambda(fieldFunction).getImplClass().replaceAll("/", ".");
  }

  /**
   * 获取lambda表达式对应的方法引用类名.
   *
   * @param fieldFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(FieldFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getImplClassName(FieldFunction fieldFunction) {
    final String implClass = serializedLambda(fieldFunction).getImplClass().replaceAll("/", ".");
    return implClass.substring(implClass.lastIndexOf(".") + 1);
  }

  /**
   * Serialized beanFunction serialized beanFunction.
   *
   * @param fieldFunction the beanFunction
   * @return the serialized beanFunction
   */
  public static SerializedLambda serializedLambda(FieldFunction fieldFunction) {
    if (!fieldFunction.getClass().isSynthetic()) {
      throw new CommonException("不支持非lambda表达式");
    }
    return Optional.ofNullable(LAMBDA_MAP.get(fieldFunction.getClass()))
        .map(WeakReference::get)
        .orElseGet(() -> getSerializedLambda(fieldFunction));
  }

  private static SerializedLambda getSerializedLambda(final FieldFunction fieldFunction) {
    try {
      final Class<? extends FieldFunction> clazz = fieldFunction.getClass();
      final Method writeReplace = clazz.getDeclaredMethod("writeReplace");
      if (!writeReplace.isAccessible()) {
        writeReplace.setAccessible(true);
      }
      final SerializedLambda serializedLambda =
          (SerializedLambda) writeReplace.invoke(fieldFunction);
      LAMBDA_MAP.put(clazz, new WeakReference<>(serializedLambda));
      return serializedLambda;
    } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      throw new CommonException(e);
    }
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    GetterFunction<TestLambda, Long> getterFunction = TestLambda::getId;
    SetterFunction<TestLambda, Long> setterFunction = TestLambda::setId;
    log.info("main:getMethodName[{}]", getMethodName(getterFunction));
    log.info("main:getActualTypePath:[{}]" + getImplClassFullPath(setterFunction));
  }
}
