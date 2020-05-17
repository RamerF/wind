package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import java.lang.reflect.*;
import java.sql.PreparedStatement;
import java.util.Optional;
import lombok.AllArgsConstructor;
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
    return Optional.ofNullable(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJavaTypeConverter(valueType))
        .map(converter -> converter.covertFromJdbc(valueType.originVal, parameterType))
        .orElse(valueType.originVal);
  }

  public static Object toJdbcValue(final ValueType valueType, final PreparedStatement ps) {
    return Optional.ofNullable(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJdbcTypeConverter(valueType))
        .map(converter -> converter.convertToJdbc(valueType.originVal, ps))
        .orElse(valueType.originVal);
  }

  @AllArgsConstructor(staticName = "of")
  public static class ValueType {
    /** 原始值,可能是java值或数据库值 */
    @Getter private final Object originVal;
    /** 泛型参数类型,可能是{@link Field#getGenericType()}或者{@link Method#getGenericParameterTypes()}[0] */
    @Getter private final Type genericParameterType;
  }
}
