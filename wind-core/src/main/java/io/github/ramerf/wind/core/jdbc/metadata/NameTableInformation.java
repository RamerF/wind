package io.github.ramerf.wind.core.jdbc.metadata;

import java.util.*;
import lombok.Data;

/**
 * 表名与数据库表信息映射.
 *
 * @author ramer
 * @since 2020.08.20
 */
@Data
public class NameTableInformation {
  private Map<String, TableInformation> tables = new HashMap<>();

  public void addTableInformation(TableInformation tableInformation) {
    tables.put(tableInformation.getName(), tableInformation);
  }

  public TableInformation getTableInformation(TableInformation table) {
    return tables.get(table.getName());
  }

  public TableInformation getTableInformation(String tableName) {
    return tables.get(tableName);
  }

  public List<TableInformation> getTableInformations() {
    return new ArrayList<>(tables.values());
  }
}
