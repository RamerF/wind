package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:Long <=> jdbc:Timestamp}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class LongTimestampTypeHandler implements ITypeHandler<Long, Timestamp> {
  @Override
  public Object convertToJdbc(
      Long javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal == null ? 0 : javaVal);
  }

  @Override
  public Long covertFromJdbc(final Timestamp jdbcVal, final Class<? extends Long> clazz) {
    return Objects.nonNull(jdbcVal) ? jdbcVal.getTime() : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
