package io.github.ramerf.wind.core.converter;

import java.sql.*;
import java.util.*;

/**
 * java:List&lt;Long&gt; &lt;-&gt; jdbc:Long[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListIntegerArrayTypeConverter implements TypeConverter<List<Integer>, Integer[]> {
  @Override
  public Object convertToJdbc(List<Integer> javaVal, final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf("int", javaVal.toArray());
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
}
