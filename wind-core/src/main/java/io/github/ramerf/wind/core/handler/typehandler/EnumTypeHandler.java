package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.domain.InterEnum;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import javax.annotation.Nonnull;

/**
 * {@literal java:InterEnum <=> jdbc:Integer}.
 *
 * @author ramer
 * @since 2020/3/4
 */
@SuppressWarnings("rawtypes")
public class EnumTypeHandler implements ITypeHandler<InterEnum, Object> {
  @Override
  public Object convertToJdbc(
      InterEnum interEnum, final Field field, @Nonnull final PreparedStatement ps) {
    return interEnum != null ? interEnum.value() : null;
  }

  @Override
  @SuppressWarnings("unchecked")
  public InterEnum convertFromJdbc(
      final Object value, final Object defaultValue, final Field field) {
    return value != null ? InterEnum.ofNullable(value, (Class<InterEnum>) field.getType()) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
