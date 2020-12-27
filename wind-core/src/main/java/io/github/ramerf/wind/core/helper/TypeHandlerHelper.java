package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerRegistryFactory;
import io.github.ramerf.wind.core.handler.typehandler.ITypeHandler;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.util.Objects;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * The type Type handler helper.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
@SuppressWarnings("unchecked")
@Slf4j
@Component
public class TypeHandlerHelper {
  private static TypeHandlerRegistryFactory typeHandlerRegistryFactory;

  public TypeHandlerHelper(final TypeHandlerRegistryFactory typeHandlerRegistryFactory) {
    TypeHandlerHelper.typeHandlerRegistryFactory = typeHandlerRegistryFactory;
  }

  public static Object toJavaValue(
      final ValueType valueType, final Object defaultValue, final Field field) {
    @SuppressWarnings("rawtypes")
    final ITypeHandler typeHandler = typeHandlerRegistryFactory.getToJavaTypeHandler(valueType);
    if (log.isTraceEnabled()) {
      log.trace(
          "toJavaValue:match typeHandler[typeHandler:{},field:{}]",
          Objects.isNull(typeHandler) ? null : typeHandler.getClass().getSimpleName(),
          valueType.originVal);
    }
    return typeHandler == null
        ? valueType.originVal
        : typeHandler.convertFromJdbc(valueType.originVal, defaultValue, field);
  }

  public static Object toJdbcValue(final ValueType valueType, final PreparedStatement ps) {
    @SuppressWarnings("rawtypes")
    final ITypeHandler typeHandler = typeHandlerRegistryFactory.getToJdbcTypeHandler(valueType);
    return typeHandler == null
        ? valueType.originVal
        : typeHandler.convertToJdbc(valueType.originVal, valueType.field, ps);
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

    public static ValueType of(final Object value) {
      return new ValueType(value, null, null);
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
