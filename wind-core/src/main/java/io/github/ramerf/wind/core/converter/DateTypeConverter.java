package io.github.ramerf.wind.core.converter;

import java.sql.PreparedStatement;
import java.util.Date;
import java.util.Objects;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class DateTypeConverter implements TypeConverter<Date, java.sql.Date> {
  @Override
  public Object convertToJdbc(Date javaVal, final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal.getTime());
  }

  @Override
  public Date covertFromJdbc(final java.sql.Date jdbcVal, final Class<? extends Date> clazz) {
    return Objects.nonNull(jdbcVal) ? new Date(jdbcVal.getTime()) : null;
  }
}