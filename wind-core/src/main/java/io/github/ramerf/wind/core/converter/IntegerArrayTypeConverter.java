package io.github.ramerf.wind.core.converter;

import java.sql.PreparedStatement;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class IntegerArrayTypeConverter implements TypeConverter<Integer[], Integer[]> {
  @Override
  public Integer[] convertToJdbc(Integer[] javaVal, final PreparedStatement ps) {
    return javaVal;
  }

  @Override
  public Integer[] covertFromJdbc(final Integer[] jdbcVal, final Class<? extends Integer[]> clazz) {
    return jdbcVal;
  }
}
