package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import io.github.ramerf.wind.core.factory.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.function.BeanFunction;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.util.Objects;
import java.util.Optional;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Type handler helper.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
@SuppressWarnings("unchecked")
@Slf4j
public class TypeHandlerHelper {
  @SuppressWarnings("rawtypes")
  public static Object toJavaValue(final ValueType valueType, final Class<?> parameterType) {
    return Optional.of(AppContextInject.getBean(TypeHandlerRegistryFactory.class))
        .map(
            o -> {
              final ITypeHandler typeHandler = o.getToJavaTypeHandler(valueType);
              if (log.isTraceEnabled()) {
                log.trace(
                    "toJavaValue:match typeHandler[typeHandler:{},field:{}]",
                    Objects.isNull(typeHandler) ? null : typeHandler.getClass().getSimpleName(),
                    valueType.originVal);
              }
              return typeHandler;
            })
        .map(typeHandler -> typeHandler.covertFromJdbc(valueType.originVal, parameterType))
        .orElse(valueType.originVal);
  }

  public static Object toJdbcValue(final ValueType valueType, final PreparedStatement ps) {
    return Optional.of(AppContextInject.getBean(TypeHandlerRegistryFactory.class))
        .map(o -> o.getToJdbcTypeHandler(valueType))
        .map(typeHandler -> typeHandler.convertToJdbc(valueType.originVal, valueType.field, ps))
        .orElse(valueType.originVal);
  }

  public static class ValueType {
    /** 原始值,可能是java值或数据库值. */
    @Getter private final Object originVal;
    /** 对应的字段,用于获取{@link TableColumn#columnDefinition()},也可以获取自定义注解. */
    @Getter private final Field field;
    /** 泛型参数类型,可能是{@link Field#getGenericType()}或者{@link Method#getGenericParameterTypes()}[0]. */
    @Getter private final Type genericParameterType;

    private ValueType(final Object originVal, final Type genericParameterType, final Field field) {
      this.originVal = originVal;
      this.genericParameterType = genericParameterType;
      this.field = field;
    }

    public static ValueType of(final Object originVal, final BeanFunction function) {
      return new ValueType(originVal, function.getGenericType(), function.getField());
    }

    public static ValueType of(final Object originVal, final Field field) {
      return new ValueType(originVal, field.getGenericType(), field);
    }

    public static ValueType of(
        final Object originVal, final Type genericParameterType, final Field field) {
      return new ValueType(originVal, genericParameterType, field);
    }
  }
}
