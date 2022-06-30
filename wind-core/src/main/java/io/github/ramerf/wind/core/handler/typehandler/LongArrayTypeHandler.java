package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Field;
import java.sql.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:Long[] <=> jdbc:Long[]}.
 *
 * @author ramer
 * @since 2020/3/4
 */
public class LongArrayTypeHandler implements ITypeHandler<Long[], Long[]> {
  @Override
  public Object convertToJdbc(
      Long[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      return ps.getConnection().createArrayOf(getArrayType(field), javaVal);
    } catch (SQLException e) {
      throw new WindException(e);
    }
  }

  @Override
  public Long[] convertFromJdbc(
      final Long[] jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal;
  }
}
