package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 15/08/2020
 */
@Slf4j
public class DefaultDdlExecutor implements DdlExecutor {
  @Override
  public void createTable(@Nonnull final EntityInfo entityInfo) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    StringBuilder sql = new StringBuilder("CREATE TABLE ").append(entityInfo.getName()).append(" ");
    for (EntityColumn column : columns) {
      if (column.getColumnDefinition() != null) {
        sql.append(column.getName()).append(column.getColumnDefinition());
      } else {
        sql.append(column.getName())
            .append(column.getSqlType())
            .append("(")
            .append(column.getSqlLengthDefinition())
            .append("(");
      }
    }
    final String columnDefinition =
        columns.stream().map(EntityColumn::getColumnDefinition).collect(Collectors.joining(",\n"));
    sql.append(columnDefinition).append(")");
    log.info("ddlCreate:[{}]", sql.toString());
  }

  @Override
  public void updateTable(@Nonnull final EntityInfo entityInfo) {}
}
