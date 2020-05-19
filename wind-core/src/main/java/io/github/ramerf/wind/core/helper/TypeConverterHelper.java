package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import io.github.ramerf.wind.core.function.BeanFunction;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.util.Optional;
import javax.persistence.Column;
import lombok.Getter;

/**
 * The type Type converter helper.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
@SuppressWarnings("unchecked")
public class TypeConverterHelper {
  public static Object toJavaValue(final ValueType valueType, final Class<?> parameterType) {
    return Optional.of(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJavaTypeConverter(valueType))
        .map(converter -> converter.covertFromJdbc(valueType.originVal, parameterType))
        .orElse(valueType.originVal);
  }

  public static Object toJdbcValue(final ValueType valueType, final PreparedStatement ps) {
    return Optional.of(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJdbcTypeConverter(valueType))
        .map(converter -> converter.convertToJdbc(valueType.originVal, valueType.field, ps))
        .orElse(valueType.originVal);
  }

  public static class ValueType {
    /** 原始值,可能是java值或数据库值 */
    @Getter private final Object originVal;
    /** 对应的字段,用于获取{@link Column#columnDefinition()},也可以获取自定义注解 */
    @Getter private final Field field;
    /** 泛型参数类型,可能是{@link Field#getGenericType()}或者{@link Method#getGenericParameterTypes()}[0] */
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

    public static ValueType of(final Object originVal, final Type genericParameterType) {
      return new ValueType(originVal, genericParameterType, null);
    }
  }
}
