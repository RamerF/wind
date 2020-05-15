package io.github.ramerf.wind.core.converter;

import java.sql.*;
import java.util.*;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListStringArrayTypeConverter implements TypeConverter<List<String>, String[]> {
  @Override
  public Object convertToJdbc(List<String> javaVal, final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf("varchar", javaVal.toArray());
    } catch (SQLException throwables) {
      throwables.printStackTrace();
    }
    return Objects.nonNull(javaVal) ? javaVal.toArray(new String[0]) : new String[0];
  }

  @Override
  public List<String> covertFromJdbc(
      final String[] jdbcVal, final Class<? extends List<String>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }
}
