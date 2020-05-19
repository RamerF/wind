package io.github.ramerf.wind.core.converter;

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
public class ListIntegerArrayTypeConverter implements TypeConverter<List<Integer>, Integer[]> {
  @Override
  public Object convertToJdbc(
      List<Integer> javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal.toArray());
    } catch (SQLException e) {
      e.printStackTrace();
    }
    return Objects.nonNull(javaVal) ? javaVal.toArray(new Integer[0]) : new Integer[0];
  }

  @Override
  public List<Integer> covertFromJdbc(
      final Integer[] jdbcVal, final Class<? extends List<Integer>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "int");
  }
}
