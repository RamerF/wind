/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.mysql;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.dialect.Dialect;
import java.sql.Types;

/**
 * An SQL dialect for MySQL (prior to 5.x).
 *
 * @author Gavin King
 */
@SuppressWarnings("deprecation")
public class MySQLDialect extends Dialect {

  private MySQLStorageEngine storageEngine;

  /** Constructs a MySQLDialect */
  public MySQLDialect() {
    super();

    String storageEngine = AppContextInject.getBean(WindConfiguration.class).getStorageEngine();
    if (storageEngine == null) {
      this.storageEngine = getDefaultMySQLStorageEngine();
    } else if ("innodb".equals(storageEngine.toLowerCase())) {
      this.storageEngine = InnoDBStorageEngine.INSTANCE;
    } else if ("myisam".equals(storageEngine.toLowerCase())) {
      this.storageEngine = MyISAMStorageEngine.INSTANCE;
    } else {
      throw new UnsupportedOperationException(
          "The " + storageEngine + " storage engine is not supported!");
    }

    registerColumnType(Types.BIT, "bit");
    registerColumnType(Types.BIGINT, "bigint");
    registerColumnType(Types.SMALLINT, "smallint");
    registerColumnType(Types.TINYINT, "tinyint");
    registerColumnType(Types.INTEGER, "integer");
    registerColumnType(Types.CHAR, "char(1)");
    registerColumnType(Types.FLOAT, "float");
    registerColumnType(Types.DOUBLE, "double precision");
    registerColumnType(Types.BOOLEAN, "bit"); // HHH-6935
    registerColumnType(Types.DATE, "date");
    registerColumnType(Types.TIME, "time");
    registerColumnType(Types.TIMESTAMP, "datetime");
    registerColumnType(Types.VARBINARY, "longblob");
    registerColumnType(Types.VARBINARY, 16777215, "mediumblob");
    registerColumnType(Types.VARBINARY, 65535, "blob");
    registerColumnType(Types.VARBINARY, 255, "tinyblob");
    registerColumnType(Types.BINARY, "binary($l)");
    registerColumnType(Types.LONGVARBINARY, "longblob");
    registerColumnType(Types.LONGVARBINARY, 16777215, "mediumblob");
    registerColumnType(Types.NUMERIC, "decimal($p,$s)");
    registerColumnType(Types.BLOB, "longblob");
    //		registerColumnType( Types.BLOB, 16777215, "mediumblob" );
    //		registerColumnType( Types.BLOB, 65535, "blob" );
    registerColumnType(Types.CLOB, "longtext");
    registerColumnType(Types.NCLOB, "longtext");
    //		registerColumnType( Types.CLOB, 16777215, "mediumtext" );
    //		registerColumnType( Types.CLOB, 65535, "text" );
    registerVarcharTypes();
  }

  protected void registerVarcharTypes() {
    registerColumnType(Types.VARCHAR, "longtext");
    //		registerColumnType( Types.VARCHAR, 16777215, "mediumtext" );
    //		registerColumnType( Types.VARCHAR, 65535, "text" );
    registerColumnType(Types.VARCHAR, 255, "varchar($l)");
    registerColumnType(Types.LONGVARCHAR, "longtext");
  }

  @Override
  public String getAddColumnString() {
    return "add column";
  }

  protected MySQLStorageEngine getDefaultMySQLStorageEngine() {
    return MyISAMStorageEngine.INSTANCE;
  }
}
