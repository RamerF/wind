package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.executor.DdlExecutor;
import io.github.ramerf.wind.core.executor.DefaultDdlExecutor;
import io.github.ramerf.wind.core.metadata.TableColumnInformation;
import io.github.ramerf.wind.core.metadata.TableInformation;
import java.util.List;
import javax.annotation.Nonnull;

/**
 * 选择合适的数据库执行器.执行ddl.
 *
 * @author ramer
 * @since 15 /08/2020
 * @see DdlExecutor
 */
public class DdlAdapter {
  /** The constant INSTANCE. */
  public static final DdlAdapter INSTANCE = new DdlAdapter();

  private static Dialect dialect;

  /**
   * Sets dialect.
   *
   * @param dialect the dialect
   */
  public static void setDialect(final Dialect dialect) {
    DdlAdapter.dialect = dialect;
  }

  /**
   * Create table.
   *
   * @param entityInfo the entity info
   */
  public void createTable(@Nonnull final EntityInfo entityInfo) {
    // 获取数据库元信息.类型,版本等
    DdlExecutor executor = null;
    executor = new DefaultDdlExecutor();
    executor.createTable(entityInfo);
  }

  /**
   * Update table.
   *
   * @param entityInfo the entity info
   * @param tableInformation the table information
   */
  public void updateTable(
      @Nonnull final EntityInfo entityInfo, final TableInformation tableInformation) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    final List<TableColumnInformation> existColumns = tableInformation.getColumns();
    StringBuilder sql = new StringBuilder();

    /*
     create table ${tableName} (
      id ${sqlType}(${length}) ${nullable} default ${defaultValue},
      big_decimal ${sqlType}(${precision}, ${scale}) default ${defaultValue},


     )${engine};
     create unique index UK_${tableName}_xxx on ${tableName}(${columnName});
     alter table ${tableName} add column
     */

    StringBuilder createTableSql = new StringBuilder();
    final String createTableString = "create table " + tableInformation.getName() + "(%s)";
    StringBuilder uniqueIndexSql = new StringBuilder();
    StringBuilder commentSql = new StringBuilder();
    columns.stream()
        .filter(column -> !existColumns.contains(TableColumnInformation.of(column.getName())))
        .forEach(
            column -> {
              createTableSql.append( column.getName()).append(" ");
              dialect.getTypeName(column.getSqlType().getSqlType(), column.getLength(), column.getPrecision(), column.getScale());
            });
  }
}
