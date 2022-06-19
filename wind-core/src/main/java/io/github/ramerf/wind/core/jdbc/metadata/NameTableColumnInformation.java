package io.github.ramerf.wind.core.jdbc.metadata;

import java.util.*;
import lombok.Data;

/**
 * 表名与列信息映射.
 *
 * @author ramer
 * @since 2020.08.20
 */
@Data
public class NameTableColumnInformation {
  private Map<String, List<TableColumnInformation>> columns = new HashMap<>();

  public void addTableColumnInformation(
      final String tableName, TableColumnInformation tableColumnInformation) {
    columns.merge(
        tableName,
        new ArrayList<>(),
        (k, v) -> {
          k.add(tableColumnInformation);
          return k;
        });
  }

  public List<TableColumnInformation> getTableColumnInformation(final String tableName) {
    return columns.get(tableName);
  }
}
