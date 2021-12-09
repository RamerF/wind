package io.github.ramerf.wind.core.metadata;

import io.github.ramerf.wind.core.dialect.Dialect;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.util.Collection;
import javax.sql.DataSource;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.support.JdbcUtils;

import static io.github.ramerf.wind.core.metadata.DbResolver.getConnection;
import static io.github.ramerf.wind.core.metadata.DbResolver.getMetaData;

/**
 * 数据库元数据.
 *
 * @author ramer
 * @since 2020.08.20
 */
@Slf4j
public class DbMetaData {
  private static volatile DbMetaData INSTANCE;
  @Getter private final Dialect dialect;
  @Getter private final String catelog;
  @Getter private final String schema;

  private final NameTableInformation tableInformations;

  private DbMetaData(DataSource dataSource, final String dialectName) {
    Connection connection = getConnection(dataSource);
    final DatabaseMetaData databaseMetaData = getMetaData(connection);
    final String catalog = DbResolver.getCatalog(connection);
    final String schema = DbResolver.getSchema(connection);
    this.catelog = catalog;
    this.schema = schema;
    this.tableInformations = DbResolver.getTables(databaseMetaData, catalog, schema);
    this.dialect =
        dialectName != null ? Dialect.getInstance(dialectName) : Dialect.getInstance(dataSource);
    JdbcUtils.closeConnection(connection);
  }

  /** 该方法返回一个单例. */
  public static DbMetaData getInstance(DataSource dataSource, final String dialect) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    synchronized (DbMetaData.class) {
      if (INSTANCE != null) {
        return INSTANCE;
      }
      return INSTANCE = new DbMetaData(dataSource, dialect);
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
