package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.time.*;
import java.util.TimeZone;
import javax.annotation.Nonnull;

/**
 * {@literal java:LocalDateTime <=> jdbc:java.sql.Timestamp}.
 *
 * @since 2020.11.17
 * @author ramer
 */
public class LocalDateTimeToTimestampTypeHandler implements ITypeHandler<LocalDateTime, Timestamp> {
  @Override
  public Object convertToJdbc(
      LocalDateTime javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal == null ? 0 : javaVal.atZone(ZoneId.systemDefault()).toEpochSecond();
  }

  @Override
  public LocalDateTime convertFromJdbc(
      final Timestamp jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal != null
        ? LocalDateTime.ofInstant(
            Instant.ofEpochMilli(jdbcVal.getTime()), TimeZone.getDefault().toZoneId())
        : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return "bigint";
  }
}
