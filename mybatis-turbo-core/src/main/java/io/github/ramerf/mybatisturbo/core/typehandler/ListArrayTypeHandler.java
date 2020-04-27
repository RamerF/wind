package io.github.ramerf.mybatisturbo.core.typehandler;

import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.*;

/**
 * @author Tang Xiaofeng
 * @version 2019/11/6
 */
@Slf4j
@MappedTypes(value = {List.class, ArrayList.class})
@MappedJdbcTypes(value = JdbcType.ARRAY, includeNullJdbcType = true)
public class ListArrayTypeHandler extends BaseTypeHandler<List<Object>> {

  @Override
  public void setNonNullParameter(
      PreparedStatement ps, int i, List<Object> parameter, JdbcType jdbcType) throws SQLException {
    String typeName = "bigint";
    if (Objects.nonNull(parameter)) {
      final Object param = parameter.get(0);
      if (param instanceof Long) {
        typeName = "bigint";
      } else if (param instanceof Integer) {
        typeName = "integer";
      } else if (param instanceof String) {
        typeName = "text";
      }
    }
    ps.setArray(
        i,
        ps.getConnection()
            .createArrayOf(
                typeName,
                Objects.isNull(parameter) ? new Object[0] : parameter.toArray(new Object[0])));
  }

  @Override
  public List<Object> getNullableResult(ResultSet rs, String columnName) throws SQLException {
    return getArray(rs.getArray(columnName));
  }

  @Override
  public List<Object> getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
    return getArray(rs.getArray(columnIndex));
  }

  @Override
  public List<Object> getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
    return getArray(cs.getArray(columnIndex));
  }

  private List<Object> getArray(Array array) {
    if (Objects.nonNull(array)) {
      try {
        return Stream.of((Object[]) array.getArray()).collect(Collectors.toList());
      } catch (Exception e) {
        log.warn("getArray:[{}]", e.getMessage());
      }
    }
    return new ArrayList<>();
  }
}
