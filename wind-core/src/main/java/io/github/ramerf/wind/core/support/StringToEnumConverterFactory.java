package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import javax.annotation.Nonnull;
import org.springframework.core.convert.converter.Converter;
import org.springframework.core.convert.converter.ConverterFactory;
import org.springframework.util.Assert;

/**
 * 字符串转枚举.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/28
 */
public final class StringToEnumConverterFactory implements ConverterFactory<String, InterEnum<?>> {

  @Override
  @SuppressWarnings({"rawtypes", "unchecked"})
  public <T extends InterEnum<?>> Converter<String, T> getConverter(@Nonnull Class<T> targetType) {
    Class<?> enumType = targetType;
    while (!enumType.isEnum()) {
      enumType = enumType.getSuperclass();
    }
    Assert.notNull(
        enumType, () -> "The target type " + targetType.getName() + " does not refer to an enum");
    return new StringToEnum(enumType);
  }

  @SuppressWarnings("rawtypes")
  private static class StringToEnum<T extends InterEnum> implements Converter<String, T> {

    private final Class<T> enumType;

    public StringToEnum(Class<T> enumType) {
      this.enumType = enumType;
    }

    @SuppressWarnings("unchecked")
    @Override
    public T convert(String source) {
      if (source.isEmpty()) {
        // It's an empty enum identifier: reset the enum value to null.
        return null;
      }
      return (T) InterEnum.of(source.trim(), this.enumType, () -> null);
    }
  }
}
