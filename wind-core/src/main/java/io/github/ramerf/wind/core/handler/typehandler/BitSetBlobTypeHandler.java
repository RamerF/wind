package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.WindException;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.BitSet;
import javax.annotation.Nonnull;

/**
 * java:{@literal java:BitSet <=> jdbc:byte[]}.适用于支持Blob的数据库如:Mysql
 *
 * @author ramer
 * @since 2020/3/4
 */
public class BitSetBlobTypeHandler implements ITypeHandler<BitSet, byte[]> {
  @Override
  public Object convertToJdbc(
      BitSet javaVal, final Field field, @Nonnull final PreparedStatement ps) {
    if (javaVal == null) {
      return null;
    }
    try {
      final Blob blob = ps.getConnection().createBlob();
      blob.setBytes(1, javaVal.toByteArray());
      return blob;
    } catch (SQLException e) {
      throw new WindException(e);
    }
  }

  @Override
  public BitSet convertFromJdbc(
      final byte[] jdbcVal, final Object defaultValue, final Field field) {
    return BitSet.valueOf(jdbcVal);
  }
}
