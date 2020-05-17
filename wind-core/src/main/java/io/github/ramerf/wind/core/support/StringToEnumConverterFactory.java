package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.StringUtils;
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
public final class StringToEnumConverterFactory implements ConverterFactory<String, InterEnum> {

  @Override
  @SuppressWarnings({"rawtypes", "unchecked"})
  public <T extends InterEnum> Converter<String, T> getConverter(@Nonnull Class<T> targetType) {
    Class<?> enumType = targetType;
    while (!enumType.isEnum()) {
      enumType = enumType.getSuperclass();
    }
    Assert.notNull(
        enumType, () -> "The target type " + targetType.getName() + " does not refer to an enum");
    return new StringToEnum(enumType);
  }

  private static class StringToEnum<T extends InterEnum> implements Converter<String, T> {

    private final Class<T> enumType;

    public StringToEnum(Class<T> enumType) {
      this.enumType = enumType;
    }

    @Override
    public T convert(String source) {
      if (source.isEmpty()) {
        // It's an empty enum identifier: reset the enum value to null.
        return null;
      }
      final int value;
      try {
        value = Integer.parseInt(source.trim());
      } catch (NumberFormatException e) {
        throw CommonException.of(
            ResultCode.API_PARAM_INVALID.desc(
                StringUtils.firstLowercase(enumType.getSimpleName())));
      }
      return InterEnum.of(this.enumType, value, false);
    }
  }
}
