package io.github.ramerf.wind.core.function;

import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.io.Serializable;
import java.lang.invoke.SerializedLambda;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.security.SecurityProperties.User;

/**
 * 用于bean属性setter {@link SetterFunction}/getter {@link GetterFunction}方法 函数式接口
 *
 * @author ramer
 * @since 2020 5/5
 * @see SetterFunction
 * @see GetterFunction
 */
public interface FieldFunction extends Serializable {
  Logger log = LoggerFactory.getLogger(FieldFunction.class);
  Map<FieldFunction, WeakReference<Field>> LAMBDA_FIELD_MAP = new ConcurrentHashMap<>();

  /**
   * 获取实现类全路径.
   *
   * @return the impl class
   */
  default String getImplClassFullPath() {
    return LambdaUtils.getImplClassFullPath(this);
  }

  /**
   * 获取实现类名.
   *
   * @return the impl class
   */
  default String getImplClassName() {
    return LambdaUtils.getImplClassName(this);
  }

  /**
   * 获取对应的Field.
   *
   * @return the field
   */
  default Field getField() {
    return Optional.ofNullable(LAMBDA_FIELD_MAP.get(this))
        .map(Reference::get)
        .orElseGet(
            () -> {
              final SerializedLambda lambda = LambdaUtils.serializedLambda(this);
              final String methodName = lambda.getImplMethodName();
              final String classPath = getImplClassFullPath();
              final String property = BeanUtils.methodToProperty(methodName);
              Field field;
              try {
                field = BeanUtils.getClazz(classPath).getDeclaredField(property);
                LAMBDA_FIELD_MAP.put(this, new WeakReference<>(field));
              } catch (Exception ignored) {
                try {
                  field =
                      BeanUtils.getClazz(classPath)
                          .getDeclaredField("is" + StringUtils.firstUppercase(property));
                  LAMBDA_FIELD_MAP.put(this, new WeakReference<>(field));
                } catch (Exception e) {
                  log.warn(
                      "getField:cannot get field from lambda[{},{}]",
                      e.getMessage(),
                      e.getMessage());
                  log.error(e.getMessage(), e);
                  throw new IllegalArgumentException(e.getMessage(), e);
                }
              }
              return field;
            });
  }

  /**
   * 获取Field的泛型参数类型.
   *
   * @return the generic type
   */
  default Type getGenericType() {
    return getField().getGenericType();
  }

  /**
   * 获取Field的泛型参数泛型类型.
   *
   * @return the type [ ]
   */
  default Type[] getGenericTypeArgumentTypes() {
    return ((ParameterizedType) getGenericType()).getActualTypeArguments();
  }

  /**
   * 获取lambda表达式对应的数据库表列名.
   *
   * @return the column
   */
  default String getColumn() {
    // TODO WARN 可以在这里保存对象的IConsumer，避免反射调用取值
    // if (this instanceof IConsumer) {}

    return EntityHelper.getColumn(this);
  }

  @Data
  class CachedIConsumer {
    private Field field;
    private static final Map<Field, WeakReference<SetterFunction<?, ?>>> CONSUMER_MAP =
        new ConcurrentHashMap<>(new WeakHashMap<>());

    @SuppressWarnings({"rawtypes", "unchecked"})
    public static boolean invoke(final Field field, final Object t, final Object value) {
      WeakReference<SetterFunction<?, ?>> reference = CONSUMER_MAP.get(field);
      SetterFunction consumer;
      if (reference != null && (consumer = reference.get()) != null) {
        consumer.accept(t, value);
        return true;
      }
      return false;
    }

    public static void put(final SetterFunction<?, ?> setterFunction) {
      Field field = setterFunction.getField();
      Class<?> clazz = field.getDeclaringClass();
      Optional.ofNullable(CONSUMER_MAP.get(field))
          .map(Reference::get)
          .orElseGet(
              () -> {
                CONSUMER_MAP.put(field, new WeakReference<>(setterFunction));
                return null;
              });
    }

    public static void main(String args[]) throws NoSuchFieldException {
      User user = new User();
      SetterFunction<User, String> name = User::setName;
      log.info("main:[{}]", user.getName());
      //      CachedIConsumer.put(name);
      Field field = User.class.getDeclaredField("name");
      CachedIConsumer.invoke(field, user, "ramer");
      log.info("main:[{}]", user.getName());
    }
  }
}
