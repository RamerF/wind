package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
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
    columns.stream()
        .filter(column -> !existColumns.contains(TableColumnInformation.of(column.getName()))).forEach();
    for (EntityColumn column : columns) {

      throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
    }
  }
}
