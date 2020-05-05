package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IConsumer;
import java.io.*;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
public final class LambdaUtils {
  /** SerializedLambda 反序列化缓存 */
  private static final Map<Class<?>, WeakReference<SerializedLambda>> LAMBDA_MAP =
      new ConcurrentHashMap<>();

  /**
   * 获取lambda表达式对应的方法引用类名.
   *
   * @param instantiatedMethodType {@link SerializedLambda#getInstantiatedMethodType()}
   */
  public static String getActualType(final String instantiatedMethodType) {
    final StringBuilder reverse = new StringBuilder(instantiatedMethodType).reverse();
    final String substring =
        reverse.substring(reverse.lastIndexOf(Constant.DEFAULT_STRING_SEMICOLON) + 1);
    return new StringBuilder(substring.substring(0, substring.indexOf("/"))).reverse().toString();
  }

  /**
   * 获取lambda表达式对应的方法引用类名全路径.
   *
   * @param instantiatedMethodType {@link SerializedLambda#getInstantiatedMethodType()}
   */
  public static String getActualTypePath(final String instantiatedMethodType) {
    return instantiatedMethodType.substring(2, instantiatedMethodType.indexOf(")") - 1);
  }

  /**
   * 获取lambda表达式对应的方法名.
   *
   * @param function 需要解析的 lambda 对象
   * @param <T> 类型，被调用的 Function 对象的目标类型
   * @return 返回解析后的结果
   * @see LambdaUtils#serializedLambda(IFunction)
   */
  public static <T> String getMethodName(IFunction<T, ?> function) {
    Class<?> clazz = function.getClass();
    return Optional.ofNullable(LAMBDA_MAP.get(clazz))
        .map(WeakReference::get)
        .map(SerializedLambda::getImplMethodName)
        .orElseGet(
            () -> {
              SerializedLambda lambda = serializedLambda(function);
              LAMBDA_MAP.put(clazz, new WeakReference<>(lambda));
              return lambda.getImplMethodName();
            });
  }

  @SuppressWarnings("DuplicatedCode")
  public static <T, R> String getMethodName(IConsumer<T, R> function) {
    Class<?> clazz = function.getClass();
    return Optional.ofNullable(LAMBDA_MAP.get(clazz))
        .map(WeakReference::get)
        .map(SerializedLambda::getImplMethodName)
        .orElseGet(
            () -> {
              SerializedLambda lambda = serializedLambda(function);
              LAMBDA_MAP.put(clazz, new WeakReference<>(lambda));
              return lambda.getImplMethodName();
            });
  }

  public static <T extends BeanFunction> SerializedLambda serializedLambda(T lambda) {
    if (!lambda.getClass().isSynthetic()) {
      throw CommonException.of("不支持非lambda表达式");
    }
    try (ObjectInputStream objectInputStream =
        new ObjectInputStream(new ByteArrayInputStream(serialize(lambda))) {
          @Override
          protected Class<?> resolveClass(ObjectStreamClass objectStreamClass)
              throws IOException, ClassNotFoundException {
            Class<?> clazz = super.resolveClass(objectStreamClass);
            return clazz == java.lang.invoke.SerializedLambda.class
                ? SerializedLambda.class
                : clazz;
          }
        }) {
      return (SerializedLambda) objectInputStream.readObject();
    } catch (ClassNotFoundException | IOException e) {
      throw CommonException.of(e);
    }
  }

  private static <T extends BeanFunction> byte[] serialize(T lambda) {
    if (lambda == null) {
      return null;
    }
    try (ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(4096);
        ObjectOutputStream oos = new ObjectOutputStream(byteArrayOutputStream)) {
      oos.writeObject(lambda);
      oos.flush();
      return byteArrayOutputStream.toByteArray();
    } catch (IOException ex) {
      throw new IllegalArgumentException(
          "Failed to serialize object of type: " + lambda.getClass(), ex);
    }
  }

  public static void main(String[] args) {
    System.out.println(" : " + getMethodName(AbstractEntityPoJo::getId));
  }
}
