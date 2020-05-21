package io.github.ramerf.wind.core.converter;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Date;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class DateTypeConverter implements TypeConverter<Date, java.sql.Date> {
  @Override
  public Object convertToJdbc(Date javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return new java.sql.Timestamp(javaVal.getTime());
  }

  @Override
  public Date covertFromJdbc(final java.sql.Date jdbcVal, final Class<? extends Date> clazz) {
    return Objects.nonNull(jdbcVal) ? new Date(jdbcVal.getTime()) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
