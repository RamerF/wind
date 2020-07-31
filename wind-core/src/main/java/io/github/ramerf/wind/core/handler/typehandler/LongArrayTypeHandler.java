package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:Long[] <=> jdbc:Long[]}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class LongArrayTypeHandler implements ITypeHandler<Long[], Long[]> {
  @Override
  public Object convertToJdbc(
      Long[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
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
  public Long[] covertFromJdbc(final Long[] jdbcVal, final Class<? extends Long[]> clazz) {
    return jdbcVal;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "bigint");
  }
}
