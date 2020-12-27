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
@SuppressWarnings("rawtypes")
public class EnumTypeHandler implements ITypeHandler<InterEnum, Object> {
  @Override
  public Object convertToJdbc(
      InterEnum interEnum, final Field field, @Nonnull final PreparedStatement ps) {
    return Objects.nonNull(interEnum) ? interEnum.value() : null;
  }

  @Override
  @SuppressWarnings("unchecked")
  public InterEnum convertFromJdbc(
      final Object value, final Object defaultValue, final Field field) {
    return Objects.nonNull(value)
        ? InterEnum.ofNullable(value, (Class<InterEnum>) field.getType())
        : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
