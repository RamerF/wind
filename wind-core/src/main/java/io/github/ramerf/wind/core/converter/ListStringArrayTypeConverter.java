package io.github.ramerf.wind.core.converter;

import java.sql.PreparedStatement;
import java.util.*;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class ListStringArrayTypeConverter implements TypeConverter<List<String>, String[]> {
  @Override
  public String[] convertToJdbc(List<String> javaVal, final PreparedStatement ps) {
    final String[] empty = new String[0];
    return Objects.nonNull(javaVal) ? javaVal.toArray(empty) : empty;
  }

  @Override
  public List<String> covertFromJdbc(
      final String[] jdbcVal, final Class<? extends List<String>> clazz) {
    return Objects.nonNull(jdbcVal) ? Arrays.asList(jdbcVal) : new ArrayList<>();
  }
}
