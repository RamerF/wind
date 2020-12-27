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
@Skip
public class LongTimestampTypeHandler implements ITypeHandler<Long, Timestamp> {
  @Override
  public Object convertToJdbc(
      Long javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal == null ? 0 : javaVal;
  }

  @Override
  public Long convertFromJdbc(
      final Timestamp jdbcVal, final Object defaultValue, final Field field) {
    return Objects.nonNull(jdbcVal) ? jdbcVal.getTime() : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
