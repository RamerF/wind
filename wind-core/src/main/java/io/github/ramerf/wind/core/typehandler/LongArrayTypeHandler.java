package io.github.ramerf.wind.core.typehandler;

import java.sql.*;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.*;

/**
 * @author Tang Xiaofeng
 * @version 2019/11/6
 */
@Slf4j
@MappedTypes(value = Long[].class)
@MappedJdbcTypes(value = JdbcType.ARRAY, includeNullJdbcType = true)
public class LongArrayTypeHandler extends BaseTypeHandler<Long[]> {
  private static final String TYPE_NAME_LONG = "bigint";

  @Override
  public void setNonNullParameter(PreparedStatement ps, int i, Long[] parameter, JdbcType jdbcType)
      throws SQLException {
    ps.setArray(i, ps.getConnection().createArrayOf(TYPE_NAME_LONG, parameter));
  }

  @Override
  public Long[] getNullableResult(ResultSet rs, String columnName) throws SQLException {
    return getArray(rs.getArray(columnName));
  }

  @Override
  public Long[] getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
    return getArray(rs.getArray(columnIndex));
  }

  @Override
  public Long[] getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
    return getArray(cs.getArray(columnIndex));
  }

  private Long[] getArray(Array array) {
    if (Objects.nonNull(array)) {
      try {
        return (Long[]) array.getArray();
      } catch (Exception e) {
        log.warn("getArray:[{}]", e.getMessage());
      }
    }
    return null;
  }
}
