package io.github.ramerf.wind.core.function;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Optional;
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
              final Field field;
              try {
                field =
                    BeanUtils.getClazz(getImplClassFullPath())
                        .getDeclaredField(BeanUtils.methodToProperty(lambda.getImplMethodName()));
                LAMBDA_FIELD_MAP.put(this, new WeakReference<>(field));
              } catch (Exception e) {
                log.warn("getField:cannot get field from lambda[{}]", e.getMessage());
                log.error(e.getMessage(), e);
                throw CommonException.of(e.getMessage(), e);
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
   * 获取lambda表达式对应的数据库表列名.
   *
   * @return the column
   */
  default String getColumn() {
    return EntityHelper.getColumn(this);
  }
}
