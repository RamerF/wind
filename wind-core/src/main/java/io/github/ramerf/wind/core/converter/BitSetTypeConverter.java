package io.github.ramerf.wind.core.converter;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.BitSet;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * java:List&lt;String&gt; &lt;-&gt; jdbc:String[].
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class BitSetTypeConverter implements TypeConverter<BitSet, byte[]> {
  @Override
  public Object convertToJdbc(
      BitSet javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal.toByteArray();
  }

  @Override
  public BitSet covertFromJdbc(final byte[] jdbcVal, final Class<? extends BitSet> clazz) {
    return Objects.nonNull(jdbcVal) ? BitSet.valueOf(jdbcVal) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
