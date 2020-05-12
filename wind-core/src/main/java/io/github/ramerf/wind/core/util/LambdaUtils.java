package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.*;
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
   * @see #serializedLambda(BeanFunction)
   * @see SerializedLambda#getInstantiatedMethodType()
   */
  public static String getActualTypePath(final String instantiatedMethodType) {
    return instantiatedMethodType
        .substring(2, instantiatedMethodType.indexOf(";"))
        .replaceAll("/", ".");
  }

  /**
   * 获取lambda表达式对应的方法名.
   *
   * @param function 需要解析的 lambda 对象
   * @return 返回解析后的结果
   * @see LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getMethodName(BeanFunction function) {
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

  public static SerializedLambda serializedLambda(BeanFunction lambda) {
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

  private static byte[] serialize(BeanFunction lambda) {
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
    IFunction<AbstractEntityPoJo, Long> function = AbstractEntityPoJo::getId;
    IConsumer<AbstractEntityPoJo, Long> consumer = AbstractEntityPoJo::setId;
    System.out.println("main:" + getMethodName(function));
    System.out.println(
        "main:getActualTypePath"
            + getActualTypePath(serializedLambda(function).getInstantiatedMethodType()));
    System.out.println(
        "main:getActualTypePath"
            + getActualTypePath(serializedLambda(consumer).getInstantiatedMethodType()));
  }
}
