package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import javax.annotation.Nonnull;

/**
 * java:List&lt;Long&gt; &lt;-&gt; jdbc:Long[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListLongArrayTypeHandler implements ITypeHandler<List<Long>, Long[]> {
  @Override
  public Object convertToJdbc(
      List<Long> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal.toArray());
    } catch (SQLException e) {
      e.printStackTrace();
    }
    return Objects.nonNull(javaVal) ? javaVal.toArray(new Long[0]) : new Long[0];
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
