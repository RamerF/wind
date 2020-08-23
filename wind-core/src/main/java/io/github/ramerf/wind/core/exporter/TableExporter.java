package io.github.ramerf.wind.core.exporter;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindContext;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 15/08/2020
 */
@Slf4j
public class TableExporter {
  private final WindContext windContext;

  private TableExporter(WindContext windContext) {
    this.windContext = windContext;
  }

  public static TableExporter of(WindContext windContext) {
    return new TableExporter(windContext);
  }

  public void createTable(@Nonnull final EntityInfo entityInfo) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    StringBuilder sql = new StringBuilder("CREATE TABLE ");
    sql.append(entityInfo.getName()).append("(\n\t");
    final String columnDefinition =
        columns.stream()
            .filter(EntityColumn::isSupported)
            .map(column -> column.getColumnDefinition(windContext.getDbMetaData().getDialect()))
            .collect(Collectors.joining(",\n\t"));
    sql.append(columnDefinition).append(",\n\t");
    final List<EntityColumn> keys = entityInfo.getPrimaryKeys();
    sql.append("primary key (")
        .append(keys.stream().map(EntityColumn::getName).collect(Collectors.joining(",")))
        .append(")\n\t)");
    log.warn("ddlCreate:[\n{}\n]", sql.toString());
    // 唯一索引
    // 列注释
    final String commentDefinition =
        columns.stream()
            .filter(EntityColumn::isSupported)
            .map(column -> column.getComment(windContext.getDbMetaData().getDialect()))
            .filter(StringUtils::nonEmpty)
            .map(
                comment ->
                    "comment on column "
                        + entityInfo.getName()
                        + '.'
                        + column.getQuotedName(dialect)
                        + " is '"
                        + columnComment
                        + "'")
            .collect(Collectors.joining(";\n\t"));

    windContext.getJdbcTemplateExecutor().getJdbcTemplate().execute(sql.toString());
  }

  public void updateTable(@Nonnull final EntityInfo entityInfo) {}
}
