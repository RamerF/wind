package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
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
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal);
    } catch (SQLException e) {
      throw new CommonException(e);
    }
  }

  @Override
  public Long[] convertFromJdbc(
      final Long[] jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "bigint");
  }
}
