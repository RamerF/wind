package io.github.ramerf.wind.core.function;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 用于bean方法(set/get) 函数式接口
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/5
 */
public interface BeanFunction extends Serializable {
  Logger log = LoggerFactory.getLogger(BeanFunction.class);
  Comparator<BeanFunction> COMPARATOR = (o1, o2) -> o1.getField().equals(o2.getField()) ? 0 : 1;
  Map<BeanFunction, WeakReference<Field>> LAMBDA_FIELD_MAP = new ConcurrentHashMap<>();

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
                  throw CommonException.of(e.getMessage(), e);
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
}
