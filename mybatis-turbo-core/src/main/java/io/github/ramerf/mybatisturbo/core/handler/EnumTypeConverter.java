package io.github.ramerf.mybatisturbo.core.handler;

import io.github.ramerf.mybatisturbo.core.entity.enums.InterEnum;
import java.util.Objects;

/**
 * 枚举类转换.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class EnumTypeConverter implements TypeConverter<InterEnum, Integer> {
  @Override
  public Integer convertToJdbc(InterEnum interEnum) {
    return Objects.nonNull(interEnum) ? interEnum.value() : null;
  }

  @Override
  public InterEnum covertFromJdbc(final Integer value, final Class<? extends InterEnum> clazz) {
    return Objects.nonNull(value) ? InterEnum.of(clazz, value) : null;
  }
}
