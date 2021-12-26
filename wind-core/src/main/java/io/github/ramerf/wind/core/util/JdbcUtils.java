package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.executor.DataAccessException;
import java.sql.*;

/**
 * @author ramer
 * @since 26/12/2021
 */
public class JdbcUtils {
  public static String getColumnName(ResultSetMetaData resultSetMetaData, int columnIndex)
      throws SQLException {
    String name = resultSetMetaData.getColumnLabel(columnIndex);
    if (!StringUtils.hasLength(name)) {
      name = resultSetMetaData.getColumnName(columnIndex);
    }
    return name;
  }

  public static void setObject(PreparedStatement ps, final int index, final Object value) {
    try {
      ps.setObject(index, value);
    } catch (SQLException e) {
      DataSourceUtils.release(ps);
      throw new DataAccessException(
          String.format("Fail to set value [index:%s,value:%s]", index, value));
    }
  }
}
