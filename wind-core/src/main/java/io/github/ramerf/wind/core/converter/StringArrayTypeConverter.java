package io.github.ramerf.wind.core.converter;

import java.sql.PreparedStatement;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class StringArrayTypeConverter implements TypeConverter<String[], String[]> {
  @Override
  public String[] convertToJdbc(String[] javaVal, final PreparedStatement ps) {
    return javaVal;
  }

  @Override
  public String[] covertFromJdbc(final String[] jdbcVal, final Class<? extends String[]> clazz) {
    return jdbcVal;
  }
}
