package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.Date;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:Date <=> jdbc:Timestamp}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class TimestampTypeHandler implements ITypeHandler<Date, Timestamp> {
  @Override
  public Object convertToJdbc(
      Date javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal.getTime());
  }

  @Override
  public Date convertFromJdbc(
      final Timestamp jdbcVal, final Object defaultValue, final Field field) {
    return Objects.nonNull(jdbcVal) ? new Date(jdbcVal.getTime()) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
