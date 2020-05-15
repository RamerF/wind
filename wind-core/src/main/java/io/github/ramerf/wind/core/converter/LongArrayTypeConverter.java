package io.github.ramerf.wind.core.converter;

import java.sql.PreparedStatement;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class LongArrayTypeConverter implements TypeConverter<Long[], Long[]> {
  @Override
  public Long[] convertToJdbc(Long[] javaVal, final PreparedStatement ps) {
    return javaVal;
  }

  @Override
  public Long[] covertFromJdbc(final Long[] jdbcVal, final Class<? extends Long[]> clazz) {
    return jdbcVal;
  }
}
