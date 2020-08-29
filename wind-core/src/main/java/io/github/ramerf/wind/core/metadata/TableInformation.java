package io.github.ramerf.wind.core.metadata;

import java.util.ArrayList;
import java.util.List;
import lombok.Data;

/**
 * 数据库表映射信息,包含列名.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.20
 */
@Data
public class TableInformation {
  private String catalog;
  private String schema;
  /** 表名. */
  private String name;

  private List<TableColumnInformation> columns = new ArrayList<>();
}
