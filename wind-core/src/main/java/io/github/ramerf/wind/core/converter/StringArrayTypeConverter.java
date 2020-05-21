package io.github.ramerf.wind.core.converter;

import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class StringArrayTypeConverter implements TypeConverter<String[], String[]> {
  @Override
  public Object convertToJdbc(
      String[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    try {
      return ps.getConnection().createArrayOf(getJdbcType(field), javaVal);
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return Objects.nonNull(javaVal) ? javaVal : new Integer[0];
  }

  @Override
  public String[] covertFromJdbc(final String[] jdbcVal, final Class<? extends String[]> clazz) {
    return jdbcVal;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "varchar");
  }
}
