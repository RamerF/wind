package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.util.JdbcUtils;
import java.sql.*;
import java.util.LinkedHashMap;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

/**
 * @since 2021.12.26
 * @author ramer
 */
@Slf4j
public class MapResultHandler extends AbstractResultHandler<Map<String, Object>> {

  @Override
  public Map<String, Object> handle(final ResultSet rs) throws SQLException {
    Map<String, Object> map = new LinkedHashMap<>();
    ResultSetMetaData rsmd = rs.getMetaData();
    int columnCount = rsmd.getColumnCount();
    for (int index = 1; index <= columnCount; index++) {
      final String column = JdbcUtils.getColumnName(rsmd, index);
      Object value = rs.getObject(index);
      map.put(column, value);
    }
    return map;
  }
}
