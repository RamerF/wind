package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.function.FieldFunction;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * The type Type handler helper.
 *
 * @author ramer
 * @since 2020/5/12
 */
@Slf4j
public class TypeHandlerHelper {

  @SuppressWarnings("unchecked")
  public static Object toJavaValue(
      final ValueType valueType, final Object defaultValue, final Field field) {
    @SuppressWarnings("rawtypes")
    final ITypeHandler typeHandler = TypeHandlerRegistryFactory.getToJavaTypeHandler(valueType);
    if (log.isTraceEnabled()) {
      log.trace(
          "toJavaValue:match typeHandler[typeHandler:{},field:{}]",
          typeHandler == null ? null : typeHandler.getClass().getSimpleName(),
          valueType.originVal);
    }
    return typeHandler == null
        ? valueType.originVal
        : typeHandler.convertFromJdbc(valueType.originVal, defaultValue, field);
  }

  @SuppressWarnings("unchecked")
  public static Object toJdbcValue(final ValueType valueType, final PreparedStatement ps) {
    @SuppressWarnings("rawtypes")
    final ITypeHandler typeHandler = TypeHandlerRegistryFactory.getToJdbcTypeHandler(valueType);
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

    public static ValueType of(final Object originVal, final FieldFunction function) {
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
