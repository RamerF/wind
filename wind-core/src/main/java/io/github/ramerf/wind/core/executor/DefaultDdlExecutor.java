package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
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
  private final Dialect dialect;

  public DefaultDdlExecutor(Dialect dialect) {
    this.dialect = dialect;
  }

  @Override
  public void createTable(@Nonnull final EntityInfo entityInfo) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    StringBuilder sql = new StringBuilder("CREATE TABLE ").append(entityInfo.getName()).append(" ");
    for (EntityColumn column : columns) {
      final String columnDefinition = column.getColumnDefinition(dialect);
      if (columnDefinition != null) {
        sql.append(column.getName()).append(columnDefinition);
      } else {
        sql.append(column.getName())
            .append(column.getJavaType())
            .append("(")
            .append(column.getSqlLengthDefinition())
            .append("(");
      }
    }
    final String columnDefinition =
        columns.stream()
            .map(column -> column.getColumnDefinition(dialect))
            .collect(Collectors.joining(",\n"));
    sql.append(columnDefinition).append(")");
    log.warn("ddlCreate:[{}]", sql.toString());
  }

  @Override
  public void updateTable(@Nonnull final EntityInfo entityInfo) {}

  @Override
  public Dialect getDialect() {
    return dialect;
  }
}
