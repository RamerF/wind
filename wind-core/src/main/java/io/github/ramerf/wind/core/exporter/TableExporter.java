package io.github.ramerf.wind.core.exporter;

import io.github.ramerf.wind.WindContext;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.dialect.sqlite.SqliteDialect;
import io.github.ramerf.wind.core.executor.*;
import io.github.ramerf.wind.core.jdbc.session.TransactionIsolationLevel;
import io.github.ramerf.wind.core.metadata.TableColumnInformation;
import io.github.ramerf.wind.core.metadata.TableInformation;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * 自动建表.
 *
 * @since 2020.10.28
 * @author ramer
 */
@Slf4j
public class TableExporter {
  private Executor executor;
  private final WindContext windContext;
  private final Dialect dialect;

  private TableExporter(WindContext windContext) {
    this.windContext = windContext;
    this.dialect = windContext.getDbMetaData().getDialect();
  }

  public static TableExporter of(WindContext windContext) {
    final Configuration configuration = windContext.getConfiguration();
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    final DataSource dataSource = jdbcEnvironment.getDataSource();

    final TableExporter tableExporter = new TableExporter(windContext);
    tableExporter.executor =
        new SimpleJdbcExecutor(
            configuration,
            jdbcEnvironment
                .getTransactionFactory()
                .newTransaction(dataSource, TransactionIsolationLevel.READ_COMMITTED, true));
    return tableExporter;
  }

  /** 先删除,再创建. */
  public void createTable(@Nonnull final EntityInfo entityInfo) {
    // 删除已存在的表
    {
      final String dropSql = "drop table if exists " + entityInfo.getName();
      log.info("ddlAuto:drop table[{}]", dropSql);
      try {
        executor.update(dropSql, ps -> {});
      } catch (DataAccessException e) {
        log.warn("Fail to drop table:" + entityInfo.getName(), e);
      }
    }
    // 列信息
    {
      final List<EntityColumn> columns = entityInfo.getEntityColumns();
      StringBuilder sql = new StringBuilder("create table ");
      sql.append(entityInfo.getName()).append("(\n\t");
      final String columnDefinition =
          columns.stream()
              .filter(EntityColumn::isSupported)
              .sorted(Comparator.comparing(EntityColumn::isPrimaryKey).reversed())
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
        if (dialect instanceof SqliteDialect) {
          // sqlite no table comment
        } else {
          sql.append(" comment ").append("'").append(entityComment).append("'");
        }
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
        log.debug("createTable:columnComment[\n\t{}\n]", columnComment);
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
      log.info("createTable:[\n{}\n]", sql);
      try {
        executor.update(sql.toString(), ps -> {});
      } catch (DataAccessException e) {
        log.warn("Fail to create table:" + entityInfo.getName(), e);
      }
    }
    // 索引
    {
      StringBuilder sql = new StringBuilder();
      final List<EntityIndex> entityIndexes = entityInfo.getEntityIndexes();
      if (!entityIndexes.isEmpty()) {
        for (EntityIndex entityIndex : entityIndexes) {
          String sqlDefinition = entityIndex.getSqlDefinition(dialect);
          sql.append(sqlDefinition).append(";\n");
        }
        log.info("createTable:index[\n{}]", sql);
        try {
          executor.update(sql.toString(), ps -> {});
        } catch (DataAccessException e) {
          log.warn("Fail to create index of table:" + entityInfo.getName(), e);
        }
      }
    }
  }

  /** 仅新增列，不支持更新列定义 */
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
    log.info("updateTable:[\n{}\n]", sql);
    try {
      executor.update(sql.toString(), ps -> {});
    } catch (DataAccessException e) {
      log.warn("Fail to execute ddl of table:" + entityInfo.getName() + ", sql:" + sql, e);
    }
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
        .filter(EntityColumn::isSupported)
        .collect(Collectors.toList());
  }
}
