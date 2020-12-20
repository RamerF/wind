package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Date;
import javax.annotation.Nonnull;

/**
 * {@literal java:Date <=> jdbc:java.sql.Date}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class DateTypeHandler implements ITypeHandler<Date, java.sql.Date> {
  @Override
  public Object convertToJdbc(
      Date javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal.getTime());
  }

  @Override
  public Date covertFromJdbc(final java.sql.Date jdbcVal, final Class<? extends Date> clazz) {
    return jdbcVal != null ? new Date(jdbcVal.getTime()) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
