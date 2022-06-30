package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Field;
import java.sql.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:Integer[] <=> jdbc:Integer[]}.
 *
 * @author ramer
 * @since 2020/3/4
 */
public class IntegerArrayTypeHandler implements ITypeHandler<Integer[], Integer[]> {
  @Override
  public Object convertToJdbc(
      Integer[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
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
  public Integer[] convertFromJdbc(
      final Integer[] jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal;
  }
}
