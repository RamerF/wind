package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.time.*;
import java.util.Objects;
import java.util.TimeZone;
import javax.annotation.Nonnull;

/**
 * {@literal java:LocalDateTime <=> jdbc:Integer}.
 *
 * @since 2020.11.17
 * @author Tang Xiaofeng
 */
public class LocalDateTimeToIntegerTypeHandler implements ITypeHandler<LocalDateTime, Integer> {
  @Override
  public Object convertToJdbc(
      LocalDateTime javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal == null ? 0 : javaVal.atZone(ZoneId.systemDefault()).toEpochSecond();
  }

  @Override
  public LocalDateTime covertFromJdbc(
      final Integer jdbcVal, final Class<? extends LocalDateTime> clazz) {
    return Objects.nonNull(jdbcVal)
        ? LocalDateTime.ofInstant(Instant.ofEpochSecond(jdbcVal), TimeZone.getDefault().toZoneId())
        : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return "bigint";
  }
}
