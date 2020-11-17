package io.github.ramerf.wind.core.metadata;

import io.github.ramerf.wind.core.config.WindConfiguration.DdlAuto;
import io.github.ramerf.wind.core.dialect.Dialect;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.util.Collection;
import javax.sql.DataSource;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.metadata.DbResolver.getConnection;
import static io.github.ramerf.wind.core.metadata.DbResolver.getMetaData;

/**
 * 数据库元数据.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.20
 */
@Slf4j
public class DbMetaData {
  private static volatile DbMetaData INSTANCE;
  @Getter private final Dialect dialect;
  @Getter private String catelog;
  @Getter private String schema;

  private NameTableInformation tableInformations;

  private DbMetaData(DataSource dataSource, final String dialectName, final DdlAuto ddlAuto) {
    if (!ddlAuto.equals(DdlAuto.NONE)) {
      Connection connection = getConnection(dataSource);
      final DatabaseMetaData databaseMetaData = getMetaData(connection);
      final String catalog = DbResolver.getCatalog(connection);
      final String schema = DbResolver.getSchema(connection);
      this.catelog = catalog;
      this.schema = schema;
      this.tableInformations = DbResolver.getTables(databaseMetaData, catalog, schema);
    }
    this.dialect =
        dialectName != null ? Dialect.getInstance(dialectName) : Dialect.getInstance(dataSource);
  }

  /** 该方法返回一个单例. */
  public static DbMetaData getInstance(
      DataSource dataSource, final String dialect, final DdlAuto ddlAuto) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    synchronized (DbMetaData.class) {
      if (INSTANCE != null) {
        return INSTANCE;
      }
      return INSTANCE = new DbMetaData(dataSource, dialect, ddlAuto);
    }
  }

  /** 获取所有表信息. */
  public Collection<TableInformation> getTableInformations() {
    return this.tableInformations.getTables().values();
  }

  /** 获取表信息. */
  public TableInformation getTableInformation(final String tableName) {
    return this.tableInformations.getTables().get(tableName);
  }
}
