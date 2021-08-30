package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.BitSet;
import javax.annotation.Nonnull;

/**
 * java:{@literal java:BitSet <=> jdbc:byte[]}.适用于支持byte[]/bytea的数据库如:Pgsql.
 *
 * @author ramer
 * @since 2020/3/4
 */
public class BitSetByteArrTypeHandler implements ITypeHandler<BitSet, byte[]> {
  @Override
  public Object convertToJdbc(
      BitSet javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    return javaVal != null ? javaVal.toByteArray() : null;
  }

  @Override
  public BitSet convertFromJdbc(
      final byte[] jdbcVal, final Object defaultValue, final Field field) {
    return jdbcVal != null ? BitSet.valueOf(jdbcVal) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
