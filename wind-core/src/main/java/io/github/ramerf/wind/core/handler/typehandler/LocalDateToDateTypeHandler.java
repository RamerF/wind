package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.time.*;
import java.util.TimeZone;
import javax.annotation.Nonnull;

/**
 * {@literal java:LocalDate <=> jdbc:java.sql.Date}.
 *
 * @since 2020.11.17
 * @author ramer
 */
public class LocalDateToDateTypeHandler implements ITypeHandler<LocalDate, Date> {
  @Override
  public Object convertToJdbc(
      LocalDate javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal == null
        ? 0
        : new Date(javaVal.atTime(LocalTime.MIN).atZone(ZoneId.systemDefault()).toEpochSecond());
  }

  @Override
  public LocalDate convertFromJdbc(
      final Date jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal != null
        ? LocalDateTime.ofInstant(
                Instant.ofEpochMilli(jdbcVal.getTime()), TimeZone.getDefault().toZoneId())
            .toLocalDate()
        : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return "DATETIME";
  }
}
