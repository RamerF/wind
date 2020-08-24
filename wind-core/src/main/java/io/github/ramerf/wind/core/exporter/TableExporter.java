package io.github.ramerf.wind.core.exporter;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindContext;
import io.github.ramerf.wind.core.dialect.Dialect;
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
    final Dialect dialect = windContext.getDbMetaData().getDialect();
    final String columnDefinition =
        columns.stream()
            .filter(EntityColumn::isSupported)
            .map(column -> column.getColumnDefinition(dialect))
            .collect(Collectors.joining(",\n\t"));
    sql.append(columnDefinition).append(",\n\t");
    final List<EntityColumn> keys = entityInfo.getPrimaryKeys();
    sql.append("primary key (")
        .append(keys.stream().map(EntityColumn::getName).collect(Collectors.joining(",")))
        .append(")\n\t)");

    final String entityComment = entityInfo.getComment();
    // 不支持comment on的数据库直接跟comment
    if (!dialect.isSupportCommentOn() && StringUtils.nonEmpty(entityComment)) {
      sql.append(" comment ").append(entityComment);
    }
    // 列注释
    if (dialect.isSupportCommentOn()) {
      final String columnComment =
          columns.stream()
              .filter(EntityColumn::isSupported)
              .filter(column -> StringUtils.nonEmpty(column.getComment()))
              .map(comment -> comment.getComment(entityInfo.getName(), dialect))
              .collect(Collectors.joining(";\n\t"));
      log.warn("ddlCreate:[\n{}\n]", columnComment);
      sql.append(";\n\t").append(columnComment);
      if (StringUtils.nonEmpty(entityComment)) {
        final String tableComment =
            dialect.getCommonOnTableString(
                windContext.getDbMetaData().getCatelog(),
                windContext.getDbMetaData().getSchema(),
                entityInfo.getName(),
                entityComment);
        sql.append(";\n\t").append(tableComment);
      }
    }
    log.warn("ddlCreate:[\n{}\n]", sql.toString());
    windContext.getJdbcTemplateExecutor().getJdbcTemplate().execute(sql.toString());
    // 唯一索引
  }

  public void updateTable(@Nonnull final EntityInfo entityInfo) {}
}
