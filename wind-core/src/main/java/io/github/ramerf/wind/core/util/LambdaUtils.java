package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.*;
import java.io.*;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Lambda utils.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/26
 */
public final class LambdaUtils {
  /** SerializedLambda 反序列化缓存 */
  private static final Map<Class<?>, WeakReference<SerializedLambda>> LAMBDA_MAP =
      new ConcurrentHashMap<>();

  /**
   * 获取lambda表达式对应的方法引用类名.
   *
   * @param implClass {@link SerializedLambda#getImplClass()}
   * @return the actual type
   */
  public static String getActualType(final String implClass) {
    return implClass.substring(implClass.lastIndexOf("/") + 1);
  }

  /**
   * 获取lambda表达式对应的方法引用类名全路径.
   *
   * @param function the IFunction
   * @return the actual type path
   * @see #serializedLambda(BeanFunction) #serializedLambda(BeanFunction)
   */
  public static String getActualTypePath(final BeanFunction function) {
    final SerializedLambda lambda = serializedLambda(function);
    return lambda.getImplClass().replaceAll("/", ".");
  }

  /**
   * 获取lambda表达式对应的方法引用类名全路径.
   *
   * @param implClass {@link SerializedLambda#getImplClass()}
   * @return the actual type path
   * @see #serializedLambda(BeanFunction) #serializedLambda(BeanFunction)
   */
  public static String getActualTypePath(final String implClass) {
    return implClass.replaceAll("/", ".");
  }

  /**
   * 获取lambda表达式对应的方法名.
   *
   * @param function 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(BeanFunction) LambdaUtils#serializedLambda(BeanFunction)
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

  /**
   * Serialized lambda serialized lambda.
   *
   * @param lambda the lambda
   * @return the serialized lambda
   */
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

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    IFunction<AbstractEntityPoJo, Long> function = AbstractEntityPoJo::getId;
    IConsumer<AbstractEntityPoJo, Long> consumer = AbstractEntityPoJo::setId;
    System.out.println("main:" + getMethodName(function));
    System.out.println(
        "main:getActualTypePath:"
            + getActualTypePath(serializedLambda(function).getInstantiatedMethodType()));
    System.out.println(
        "main:getActualTypePath:"
            + getActualTypePath(serializedLambda(consumer).getInstantiatedMethodType()));
  }
}
