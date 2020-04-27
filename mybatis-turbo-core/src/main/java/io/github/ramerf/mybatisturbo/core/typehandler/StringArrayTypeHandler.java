package io.github.ramerf.mybatisturbo.core.typehandler;

import java.sql.*;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.*;

/**
 * @author Tang Xiaofeng
 * @version 2019/11/6
 */
@Slf4j
@MappedTypes(value = String[].class)
@MappedJdbcTypes(value = JdbcType.ARRAY, includeNullJdbcType = true)
public class StringArrayTypeHandler extends BaseTypeHandler<String[]> {
  private static final String TYPE_NAME_LONG = "text";

  @Override
  public void setNonNullParameter(
      PreparedStatement ps, int i, String[] parameter, JdbcType jdbcType) throws SQLException {
    ps.setArray(i, ps.getConnection().createArrayOf(TYPE_NAME_LONG, parameter));
  }

  @Override
  public String[] getNullableResult(ResultSet rs, String columnName) throws SQLException {
    return getArray(rs.getArray(columnName));
  }

  @Override
  public String[] getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
    return getArray(rs.getArray(columnIndex));
  }

  @Override
  public String[] getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
    return getArray(cs.getArray(columnIndex));
  }

  private String[] getArray(Array array) {
    if (Objects.nonNull(array)) {
      try {
        return (String[]) array.getArray();
      } catch (Exception e) {
        log.warn("getArray:[{}]", e.getMessage());
      }
    }
    return null;
  }
}
