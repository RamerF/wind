package io.github.ramerf.wind.core.converter;

import java.sql.*;
import java.util.*;

/**
 * java:List&lt;Long&gt; &lt;-&gt; jdbc:Long[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListLongArrayTypeConverter implements TypeConverter<List<Long>, Long[]> {
  @Override
  public Object convertToJdbc(List<Long> javaVal, final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf("bigint", javaVal.toArray());
    } catch (SQLException e) {
      e.printStackTrace();
    }
    return Objects.nonNull(javaVal) ? javaVal.toArray(new Long[0]) : new Long[0];
  }

  @Override
  public List<Long> covertFromJdbc(final Long[] jdbcVal, final Class<? extends List<Long>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }
}
