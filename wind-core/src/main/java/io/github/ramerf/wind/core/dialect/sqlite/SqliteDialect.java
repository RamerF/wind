/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.sqlite;

import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupport;
import io.github.ramerf.wind.core.dialect.identity.SQLiteIdentityColumnSupport;
import io.github.ramerf.wind.core.dialect.mysql.MyISAMStorageEngine;
import io.github.ramerf.wind.core.dialect.mysql.MySQLStorageEngine;
import java.math.BigDecimal;
import java.util.Date;

/** An SQL dialect for Sqlite. */
public class SqliteDialect extends Dialect {

  /** Constructs a MySQLDialect */
  public SqliteDialect() {
    super();
    // char type
    registerColumnType(Character.class, "TEXT");
    registerColumnType(char.class, "TEXT");
    // boolean type
    registerColumnType(Boolean.class, "INTEGER");
    registerColumnType(boolean.class, "INTEGER");
    // value type
    registerColumnType(Float.class, "REAL");
    registerColumnType(float.class, "REAL");

    registerColumnType(Double.class, "REAL");
    registerColumnType(double.class, "REAL");

    registerColumnType(BigDecimal.class, "REAL");
    // date type
    registerColumnType(Date.class, "TEXT");
    // varchar type
    registerColumnType(String.class, "TEXT");
  }

  @Override
  public String getAddColumnString() {
    return "add column";
  }

  protected MySQLStorageEngine getDefaultMySQLStorageEngine() {
    return MyISAMStorageEngine.INSTANCE;
  }

  @Override
  public IdentityColumnSupport getIdentityColumnSupport() {
    return new SQLiteIdentityColumnSupport();
  }

  @Override
  public String getKeyHolderKey() {
    return "GENERATED_KEY";
  }

  public String getCurrentTimestampSelectString() {
    return "select current_timestamp";
  }
}
