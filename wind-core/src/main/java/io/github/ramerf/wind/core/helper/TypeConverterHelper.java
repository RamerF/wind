package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import java.lang.reflect.Type;
import java.sql.PreparedStatement;
import java.util.Optional;

/**
 * The type Type converter helper.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
@SuppressWarnings("unchecked")
public class TypeConverterHelper {
  public static Object toJavaValue(
      final Object originVal, final Type genericParameterType, final Class<?> parameterType) {
    return Optional.of(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJavaTypeConverter(originVal, genericParameterType))
        .map(converter -> converter.covertFromJdbc(originVal, parameterType))
        .orElse(originVal);
  }

  public static Object toJdbcValue(
      final Object originVal, final Type genericFieldType, final PreparedStatement ps) {
    return Optional.of(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(o -> o.getToJdbcTypeConverter(originVal, genericFieldType))
        .map(converter -> converter.convertToJdbc(originVal, ps))
        .orElse(originVal);
  }
}
