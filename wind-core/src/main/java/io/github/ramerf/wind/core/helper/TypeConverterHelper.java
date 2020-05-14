package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.factory.TypeConverterRegistryFactory;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Type converter helper.
 *
 * @author Tang Xiaofeng
 * @since 2020/5/12
 */
public class TypeConverterHelper {
  @SuppressWarnings("unchecked")
  public static Object toJdbcValue(final Object originVal) {
    return Optional.of(AppContextInject.getBean(TypeConverterRegistryFactory.class))
        .map(
            o ->
                o.getToJdbcTypeConverter(
                    originVal, Objects.isNull(originVal) ? null : originVal.getClass()))
        .map(converter -> converter.convertToJdbc(originVal))
        .orElse(originVal);
  }
}
