package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:InterEnum <=> jdbc:Integer}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class EnumTypeHandler implements ITypeHandler<InterEnum, Integer> {
  @Override
  public Integer convertToJdbc(InterEnum interEnum, final Field field, @Nonnull final PreparedStatement ps) {
    return Objects.nonNull(interEnum) ? interEnum.value() : null;
  }

  @Override
  public InterEnum covertFromJdbc(final Integer value, final Class<? extends InterEnum> clazz) {
    return Objects.nonNull(value) ? InterEnum.of(clazz, value) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
