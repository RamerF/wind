package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * {@literal java:List<Long> <=> jdbc:Long[]}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListLongArrayTypeHandler implements ITypeHandler<List<Long>, Long[]> {
  @Override
  public Object convertToJdbc(
      List<Long> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal.toArray());
    } catch (SQLException e) {
      throw CommonException.of(e);
    }
  }

  @Override
  public List<Long> covertFromJdbc(final Long[] jdbcVal, final Class<? extends List<Long>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "bigint");
  }
}
