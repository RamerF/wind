package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:Integer[] <=> jdbc:Integer[]}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class IntegerArrayTypeHandler implements ITypeHandler<Integer[], Integer[]> {
  @Override
  public Object convertToJdbc(
      Integer[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal);
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return Objects.nonNull(javaVal) ? javaVal : new Integer[0];
  }

  @Override
  public Integer[] covertFromJdbc(final Integer[] jdbcVal, final Class<? extends Integer[]> clazz) {
    return jdbcVal;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "int");
  }
}
