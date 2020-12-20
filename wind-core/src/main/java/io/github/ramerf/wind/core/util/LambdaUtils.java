package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.TestLambda;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.*;
import java.io.*;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Lambda utils.
 *
 * @author Tang Xiaofeng
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
   * @param beanFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(BeanFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getMethodName(BeanFunction beanFunction) {
    return serializedLambda(beanFunction).getImplMethodName();
  }

  /**
   * 获取lambda表达式对应的方法引用类名全路径.
   *
   * @param beanFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(BeanFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getImplClassFullPath(BeanFunction beanFunction) {
    return serializedLambda(beanFunction).getImplClass().replaceAll("/", ".");
  }

  /**
   * 获取lambda表达式对应的方法引用类名.
   *
   * @param beanFunction 需要解析的 lambda 对象
   * @return 返回解析后的结果 method name
   * @see LambdaUtils#serializedLambda(BeanFunction) LambdaUtils#serializedLambda(BeanFunction)
   */
  public static String getImplClassName(BeanFunction beanFunction) {
    final String implClass = serializedLambda(beanFunction).getImplClass().replaceAll("/", ".");
    return implClass.substring(implClass.lastIndexOf(".") + 1);
  }

  /**
   * Serialized beanFunction serialized beanFunction.
   *
   * @param beanFunction the beanFunction
   * @return the serialized beanFunction
   */
  public static SerializedLambda serializedLambda(BeanFunction beanFunction) {
    if (!beanFunction.getClass().isSynthetic()) {
      throw CommonException.of("不支持非lambda表达式");
    }
    return Optional.ofNullable(LAMBDA_MAP.get(beanFunction.getClass()))
        .map(WeakReference::get)
        .orElseGet(() -> getSerializedLambda(beanFunction));
  }

  private static SerializedLambda getSerializedLambda(final BeanFunction beanFunction) {
    try (ObjectInputStream objectInputStream =
        new ObjectInputStream(new ByteArrayInputStream(serialize(beanFunction))) {
          @Override
          protected Class<?> resolveClass(ObjectStreamClass objectStreamClass)
              throws IOException, ClassNotFoundException {
            Class<?> clazz = super.resolveClass(objectStreamClass);
            return clazz == java.lang.invoke.SerializedLambda.class
                ? SerializedLambda.class
                : clazz;
          }
        }) {
      final SerializedLambda serializedLambda = (SerializedLambda) objectInputStream.readObject();
      LAMBDA_MAP.put(beanFunction.getClass(), new WeakReference<>(serializedLambda));
      return serializedLambda;
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
    IFunction<TestLambda, Long> function = TestLambda::getId;
    IConsumer<TestLambda, Long> consumer = TestLambda::setId;
    log.info("main:getMethodName[{}]", getMethodName(function));
    log.info("main:getActualTypePath:[{}]" + getImplClassFullPath(consumer));
  }
}
