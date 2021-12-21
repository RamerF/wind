package io.github.ramerf.wind.core.metadata;

import io.github.ramerf.wind.core.metadata.TableIndexInformation.FlatTableIndexInformation;
import java.sql.*;
import java.util.HashSet;
import java.util.Set;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.support.JdbcUtils;

/**
 * 该类主要是简化数据库操作相关的try-catch.
 *
 * @author ramer
 * @since 2020.08.20
 */
@Slf4j
public final class DbResolver {
  @Nullable
  public static Connection getConnection(DataSource dataSource) {
    try {
      return dataSource.getConnection();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return null;
    }
  }

  public static DatabaseMetaData getMetaData(@Nullable Connection connection) {
    if (connection == null) {
      return null;
    }
    try {
      return connection.getMetaData();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      return null;
    }
  }

  public static String getCatalog(@Nullable Connection connection) {
    if (connection == null) {
      return null;
    }
    try {
      return connection.getCatalog();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      // throw new IllegalStateException(e);
      return null;
    }
  }

  public static String getSchema(@Nullable Connection connection) {
    if (connection == null) {
      return null;
    }
    try {
      return connection.getSchema();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      // throw new IllegalStateException(e);
      return null;
    }
  }

  /** 获取所有的表信息. */
  public static NameTableInformation getTables(
      @Nullable DatabaseMetaData metaData, final String catalog, final String schemaPattern) {
    if (metaData == null) {
      return new NameTableInformation();
    }
    ResultSet resultSet = null;
    try {
      resultSet = metaData.getTables(catalog, schemaPattern, "%", new String[] {"TABLE"});
      final NameTableInformation tableInformation = extractTablesResultSet(resultSet);
      populateColumns(metaData, catalog, schemaPattern, tableInformation);
      tableInformation
          .getTableInformations()
          .forEach(o -> populateIndexes(metaData, catalog, schemaPattern, o));
      return tableInformation;
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    } finally {
      JdbcUtils.closeResultSet(resultSet);
    }
    return new NameTableInformation();
  }

  /** 填充表包含的索引信息. */
  private static void populateIndexes(
      DatabaseMetaData metaData,
      final String catalog,
      final String schemaPattern,
      final TableInformation tableInformation) {
    ResultSet resultSet = null;
    try {
      resultSet =
          metaData.getIndexInfo(
              tableInformation.getCatalog(),
              tableInformation.getSchema(),
              tableInformation.getName(),
              false,
              true);
      extractTableIndexesResultSet(resultSet, tableInformation);
    } catch (SQLException e) {
      log.warn("populateColumns:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    } finally {
      JdbcUtils.closeResultSet(resultSet);
    }
  }

  /** 填充表包含的列信息. */
  private static void populateColumns(
      DatabaseMetaData metaData,
      final String catalog,
      final String schemaPattern,
      final NameTableInformation nameTableInformation) {
    ResultSet resultSet = null;
    try {
      resultSet = metaData.getColumns(catalog, schemaPattern, null, "%");
      extractTableColumnsResultSet(resultSet, nameTableInformation);
    } catch (SQLException e) {
      log.warn("populateColumns:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    } finally {
      JdbcUtils.closeResultSet(resultSet);
    }
  }

  /** @see DatabaseMetaData#getTables(String, String, String, String[]) */
  private static NameTableInformation extractTablesResultSet(final ResultSet resultSet)
      throws SQLException {
    NameTableInformation nameTableInformation = new NameTableInformation();
    while (resultSet.next()) {
      TableInformation tableInformation = new TableInformation();
      tableInformation.setCatalog(resultSet.getString(1));
      tableInformation.setSchema(resultSet.getString(2));
      tableInformation.setName(resultSet.getString(3));
      nameTableInformation.addTableInformation(tableInformation);
    }
    return nameTableInformation;
  }

  /** @see DatabaseMetaData#getIndexInfo(String, String, String, boolean, boolean) */
  private static void extractTableIndexesResultSet(
      final ResultSet resultSet, @Nonnull final TableInformation tableInformation)
      throws SQLException {
    Set<FlatTableIndexInformation> indexes = new HashSet<>();
    while (resultSet.next()) {
      final String indexName = resultSet.getString("INDEX_NAME");
      final boolean unique = !resultSet.getBoolean("NON_UNIQUE");
      final short type = resultSet.getShort("TYPE");
      final String columnName = resultSet.getString("COLUMN_NAME");
      final short position = resultSet.getShort("ORDINAL_POSITION");
      final String order = resultSet.getString("ASC_OR_DESC");
      final String indexQualifier = resultSet.getString("INDEX_QUALIFIER");
      final String tableName = resultSet.getString("TABLE_NAME");
      indexes.add(
          FlatTableIndexInformation.of(
              indexName, unique, type, columnName, position, order, tableName));
    }
    if (!indexes.isEmpty()) {
      tableInformation.setIndexes(TableIndexInformation.of(indexes));
    }
  }

  /** @see DatabaseMetaData#getColumns(String, String, String, String) */
  private static void extractTableColumnsResultSet(
      final ResultSet resultSet, final NameTableInformation nameTableInformation)
      throws SQLException {
    while (resultSet.next()) {
      final String tableName = resultSet.getString(3);
      final TableInformation tableInformation = nameTableInformation.getTableInformation(tableName);
      if (tableInformation != null) {
        tableInformation
            .getColumns()
            .add(
                TableColumnInformation.of(
                    resultSet.getString(4), //
                    resultSet.getInt(5),
                    resultSet.getString(6)));
      }
    }
  }
}
