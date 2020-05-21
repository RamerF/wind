package io.github.ramerf.wind.core.converter;

import io.github.ramerf.wind.core.helper.EntityHelper;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class LongArrayTypeConverter implements TypeConverter<Long[], Long[]> {
  @Override
  public Object convertToJdbc(
      Long[] javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    try {
      final Connection connection = ps.getConnection();
      return connection.createArrayOf(getJdbcType(field), javaVal);
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return Objects.nonNull(javaVal) ? javaVal : new Integer[0];
  }

  @Override
  public Long[] covertFromJdbc(final Long[] jdbcVal, final Class<? extends Long[]> clazz) {
    return jdbcVal;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return EntityHelper.getJdbcTypeName(field, "bigint");
  }
}
