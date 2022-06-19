package io.github.ramerf.wind.core.jdbc.metadata;

import java.util.ArrayList;
import java.util.List;
import lombok.Data;

/**
 * 数据库表映射信息,包含列名.
 *
 * @author ramer
 * @since 2020.08.20
 */
@Data
public class TableInformation {
  private String catalog;
  private String schema;
  /** 表名. */
  private String name;
  /** 列信息. */
  private List<TableColumnInformation> columns = new ArrayList<>();
  /** 索引信息. */
  private List<TableIndexInformation> indexes = new ArrayList<>();
}
