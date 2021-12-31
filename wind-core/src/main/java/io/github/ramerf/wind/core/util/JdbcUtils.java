package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.executor.DataAccessException;
import java.sql.*;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 26/12/2021
 */
@Slf4j
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

  /** 复制于: {@link org.springframework.jdbc.support.JdbcUtils#supportsBatchUpdates(Connection)} */
  public static boolean supportsBatchUpdates(Connection con) {
    try {
      DatabaseMetaData dbmd = con.getMetaData();
      if (dbmd != null) {
        if (dbmd.supportsBatchUpdates()) {
          log.debug("JDBC driver supports batch updates");
          return true;
        } else {
          log.debug("JDBC driver does not support batch updates");
        }
      }
    } catch (SQLException ex) {
      log.debug("JDBC driver 'supportsBatchUpdates' method threw exception", ex);
    }
    return false;
  }
}
