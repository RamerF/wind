package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import javax.annotation.Nonnull;

/**
 * {@literal java:Date <=> jdbc:java.sql.Date}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class DateToLocalDateTimeTypeHandler implements ITypeHandler<Date, LocalDateTime> {
  @Override
  public Object convertToJdbc(
      Date javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal.getTime());
  }

  @Override
  public Date convertFromJdbc(
      final LocalDateTime jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal != null ? Date.from(jdbcVal.atZone(ZoneId.systemDefault()).toInstant()) : null;
  }
}
