package io.github.ramerf.wind.core.handler.typehandler;

import io.github.ramerf.wind.core.exception.CommonException;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.BitSet;
import javax.annotation.Nonnull;

/**
 * java:{@literal java:BitSet <=> jdbc:byte[]}.适用于支持Blob的数据库如:Mysql
 *
 * @author Tang Xiaofeng
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
      throw CommonException.of(e);
    }
  }

  @Override
  public BitSet covertFromJdbc(final byte[] jdbcVal, final Class<? extends BitSet> clazz) {
    return BitSet.valueOf(jdbcVal);
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return "blob";
  }
}