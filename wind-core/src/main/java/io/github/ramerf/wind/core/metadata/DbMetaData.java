package io.github.ramerf.wind.core.metadata;

import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.support.DdlAdapter;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.util.Collection;
import javax.sql.DataSource;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.metadata.DbResolver.*;

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
  private final NameTableInformation tableInformations;

  private DbMetaData(DataSource dataSource) {
    Connection connection = getConnection(dataSource);
    final DatabaseMetaData databaseMetaData = getMetaData(connection);
    this.dialect = Dialect.getInstance(dataSource);
    DdlAdapter.setDialect(dialect);
    this.tableInformations =
        DbResolver.getTables(databaseMetaData, getCatalog(connection), getSchema(connection));
  }

  /** 该方法返回一个单例. */
  public static DbMetaData getInstance(DataSource dataSource) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    synchronized (DbMetaData.class) {
      if (INSTANCE != null) {
        return INSTANCE;
      }
      return INSTANCE = new DbMetaData(dataSource);
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
