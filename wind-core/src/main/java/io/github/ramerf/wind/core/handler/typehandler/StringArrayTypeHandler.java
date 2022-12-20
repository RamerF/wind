package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.WindException;

import javax.annotation.Nonnull;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * {@literal java:String[] <=> jdbc:String[]}.
 *
 * @author ramer
 * @since 2020/3/4
 */
public class StringArrayTypeHandler implements ITypeHandler<String[], String[]> {
  @Override
  public Object convertToJdbc(
      String[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      return ps.getConnection().createArrayOf(getArrayType(field, "text"), javaVal);
    } catch (SQLException e) {
      throw new WindException(e);
    }
  }

  @Override
  public String[] convertFromJdbc(
      final String[] jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal;
  }
}
