package io.github.ramerf.wind.core.exporter;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.config.WindContext;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.metadata.TableColumnInformation;
import io.github.ramerf.wind.core.metadata.TableInformation;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 自动建表.
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
@Slf4j
public class TableExporter {
  private final WindContext windContext;
  private final Dialect dialect;

  private TableExporter(WindContext windContext) {
    this.windContext = windContext;
    this.dialect = windContext.getDbMetaData().getDialect();
  }

  public static TableExporter of(WindContext windContext) {
    return new TableExporter(windContext);
  }

  public void createTable(@Nonnull final EntityInfo entityInfo) {
    final List<EntityColumn> columns = entityInfo.getEntityColumns();
    StringBuilder sql = new StringBuilder("create table ");
    sql.append(entityInfo.getName()).append("(\n\t");
    final String columnDefinition =
        columns.stream()
            .filter(EntityColumn::isSupported)
            .map(column -> column.getColumnDdl(dialect))
            .collect(Collectors.joining(",\n\t"));
    sql.append(columnDefinition).append(",\n\t");
    final List<EntityColumn> keys = entityInfo.getPrimaryKeys();
    sql.append("primary key (")
        .append(keys.stream().map(EntityColumn::getName).collect(Collectors.joining(",")))
        .append(")\n\t)");

    final String entityComment = entityInfo.getComment();
    // 不支持comment on的数据库直接跟comment
    if (!dialect.isSupportCommentOn() && StringUtils.nonEmpty(entityComment)) {
      sql.append(" comment ").append("'").append(entityComment).append("'");
    }
    // 存储引擎,针对mysql
    sql.append(dialect.getTableTypeString());
    // 列注释
    if (dialect.isSupportCommentOn()) {
      final String columnComment =
          columns.stream()
              .filter(EntityColumn::isSupported)
              .filter(column -> StringUtils.nonEmpty(column.getComment()))
              .map(comment -> comment.getComment(entityInfo.getName(), dialect))
              .collect(Collectors.joining(";\n\t"));
      log.debug("createTable:columnComment[\n{}\n]", columnComment);
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
    log.info("createTable:[\n{}\n]", sql.toString());
    windContext.getExecutor().getJdbcTemplate().execute(sql.toString());
  }

  public void updateTable(@Nonnull final EntityInfo entityInfo) {
    final TableInformation tableInformation =
        windContext.getDbMetaData().getTableInformation(entityInfo.getName());
    if (tableInformation == null) {
      createTable(entityInfo);
      return;
    }
    final List<EntityColumn> updateColumns = getUpdatingColumns(entityInfo, tableInformation);
    if (updateColumns.isEmpty()) {
      return;
    }
    StringBuilder sql = new StringBuilder("alter table ");
    sql.append(entityInfo.getName()).append("\n\t");

    final String columnDefinition =
        updateColumns.stream()
            .filter(EntityColumn::isSupported)
            .map(column -> " add column " + column.getColumnDdl(dialect))
            .collect(Collectors.joining(",\n\t"));
    sql.append(columnDefinition);
    // 列注释
    if (dialect.isSupportCommentOn()) {
      final String columnComment =
          updateColumns.stream()
              .filter(EntityColumn::isSupported)
              .filter(column -> StringUtils.nonEmpty(column.getComment()))
              .map(comment -> comment.getComment(entityInfo.getName(), dialect))
              .collect(Collectors.joining(";\n\t"));
      sql.append(";\n\t").append(columnComment);
    }
    log.info("updateTable:[\n{}\n]", sql.toString());
    windContext.getExecutor().getJdbcTemplate().execute(sql.toString());
  }

  /** 获取需要更新的列. */
  private List<EntityColumn> getUpdatingColumns(
      @Nonnull final EntityInfo entityInfo, final TableInformation tableInformation) {
    final List<TableColumnInformation> existColumns = tableInformation.getColumns();
    return entityInfo.getEntityColumns().stream()
        .filter(
            column ->
                existColumns.stream()
                    .noneMatch(existColumn -> existColumn.getName().equals(column.getName())))
        .collect(Collectors.toList());
  }
}
