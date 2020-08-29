package io.github.ramerf.wind.core.metadata;

import java.util.Objects;
import lombok.Data;

/**
 * 数据库表字段信息.
 *
 * <p>后面可能会使用到扩展列信息,如: <code>nullable, type</code>等
 *
 * @author Tang Xiaofeng
 * @since 2020.08.20
 */
@Data
public class TableColumnInformation {
  private String name;

  public static TableColumnInformation of(final String name) {
    TableColumnInformation columnInformation = new TableColumnInformation();
    columnInformation.setName(name);
    return columnInformation;
  }

  /** 根据名称匹配. */
  @Override
  public boolean equals(final Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    final TableColumnInformation that = (TableColumnInformation) o;
    return Objects.equals(name, that.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }
}
