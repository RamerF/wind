package io.github.ramerf.wind.core.metadata;

import java.sql.*;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * 该类主要是简化数据库操作相关的try-catch.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.20
 */
@Slf4j
public final class DbResolver {
  public static Connection getConnection(DataSource dataSource) {
    try {
      return dataSource.getConnection();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      throw new IllegalStateException(e);
    }
  }

  public static DatabaseMetaData getMetaData(Connection connection) {
    try {
      return connection.getMetaData();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      throw new IllegalStateException(e);
    }
  }

  public static String getCatalog(Connection connection) {
    try {
      return connection.getCatalog();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      // throw new IllegalStateException(e);
      return null;
    }
  }

  public static String getSchema(Connection connection) {
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
      DatabaseMetaData metaData, final String catalog, final String schemaPattern) {
    try {
      final NameTableInformation tableInformation =
          extractTablesResultSet(
              metaData.getTables(catalog, schemaPattern, "%", new String[] {"TABLE"}));
      populateColumns(metaData, catalog, schemaPattern, tableInformation);
      return tableInformation;
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return new NameTableInformation();
  }

  /** 填充表包含的列信息. */
  private static void populateColumns(
      DatabaseMetaData metaData,
      final String catalog,
      final String schemaPattern,
      final NameTableInformation nameTableInformation) {
    try {
      final ResultSet resultSet = metaData.getColumns(catalog, schemaPattern, null, "%");
      extractTableColumnsResultSet(resultSet, nameTableInformation);
    } catch (SQLException e) {
      log.warn("populateColumns:[{}]", e.getMessage());
      log.error(e.getMessage(), e);
    }
  }

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

  private static void extractTableColumnsResultSet(
      final ResultSet resultSet, final NameTableInformation nameTableInformation)
      throws SQLException {
    NameTableColumnInformation nameTableColumnInformation = new NameTableColumnInformation();
    while (resultSet.next()) {
      final String tableName = resultSet.getString(3);
      final TableInformation tableInformation = nameTableInformation.getTableInformation(tableName);
      if (tableInformation != null) {
        tableInformation.getColumns().add(TableColumnInformation.of(resultSet.getString(4)));
      }
    }
  }
}
