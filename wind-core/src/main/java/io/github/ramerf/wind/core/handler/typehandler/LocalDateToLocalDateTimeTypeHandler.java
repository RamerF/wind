package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.time.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:LocalDate <=> jdbc:java.sql.Date}.
 *
 * @since 2020.11.17
 * @author ramer
 */
public class LocalDateToLocalDateTimeTypeHandler implements ITypeHandler<LocalDate, LocalDateTime> {
  @Override
  public Object convertToJdbc(
      LocalDate javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal == null ? 0 : javaVal.atTime(LocalTime.MIN);
  }

  @Override
  public LocalDate convertFromJdbc(
      final LocalDateTime jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal != null ? jdbcVal.toLocalDate() : null;
  }
}
