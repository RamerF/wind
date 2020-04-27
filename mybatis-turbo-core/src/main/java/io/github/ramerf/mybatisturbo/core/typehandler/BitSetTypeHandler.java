package io.github.ramerf.mybatisturbo.core.typehandler;

import java.sql.*;
import java.util.BitSet;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.*;

/**
 * @author Tang Xiaofeng
 * @version 2019/11/6
 */
@Slf4j
@MappedTypes(value = {BitSet.class})
@MappedJdbcTypes(value = JdbcType.ARRAY, includeNullJdbcType = true)
public class BitSetTypeHandler extends BaseTypeHandler<BitSet> {
  private static final String TYPE_NAME_INTEGER = "bit";

  @Override
  public void setNonNullParameter(PreparedStatement ps, int i, BitSet parameter, JdbcType jdbcType)
      throws SQLException {
    if (Objects.nonNull(parameter)) {
      final byte[] byteArray = parameter.toByteArray();
      Byte[] bys = new Byte[byteArray.length];
      for (int j = 0; j < byteArray.length; j++) {
        bys[j] = byteArray[j];
      }
      ps.setArray(i, ps.getConnection().createArrayOf(TYPE_NAME_INTEGER, bys));
    }
  }

  @Override
  public BitSet getNullableResult(ResultSet rs, String columnName) throws SQLException {
    return getArray(rs.getArray(columnName));
  }

  @Override
  public BitSet getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
    return getArray(rs.getArray(columnIndex));
  }

  @Override
  public BitSet getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
    return getArray(cs.getArray(columnIndex));
  }

  private BitSet getArray(Array array) {
    if (Objects.nonNull(array)) {
      try {
        final Boolean[] array1 = (Boolean[]) array.getArray();
        byte[] bytes = new byte[array1.length];
        for (int i = 0; i < array1.length; i++) {
          bytes[i] = (byte) (array1[i] ? 0x1 : 0x0);
        }
        return BitSet.valueOf(bytes);
      } catch (Exception e) {
        log.warn("getArray:[{}]", e.getMessage());
      }
    }
    return null;
  }
}
