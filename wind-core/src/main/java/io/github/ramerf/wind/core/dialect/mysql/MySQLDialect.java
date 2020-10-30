/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.mysql;

import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupport;
import io.github.ramerf.wind.core.dialect.identity.MySQLIdentityColumnSupport;
import java.math.BigDecimal;
import java.util.BitSet;
import java.util.Date;

/**
 * An SQL dialect for MySQL (prior to 5.x).
 *
 * @author Gavin King
 */
public class MySQLDialect extends Dialect {
  private final MySQLStorageEngine storageEngine = getDefaultMySQLStorageEngine();

  /** Constructs a MySQLDialect */
  public MySQLDialect() {
    super();
    // char type
    registerColumnType(Character.class, "char(1)");
    registerColumnType(char.class, "char(1)");
    // boolean type
    registerColumnType(Boolean.class, "bit");
    registerColumnType(boolean.class, "bit");
    // value type
    registerColumnType(Float.class, "float");
    registerColumnType(float.class, "float");

    registerColumnType(Double.class, "double");
    registerColumnType(double.class, "double");

    registerColumnType(BigDecimal.class, "decimal($p,$s)");
    // date type
    registerColumnType(Date.class, "datetime");
    // varchar type
    registerColumnType(String.class, "varchar");
    registerColumnType(String.class, 255, "varchar($l)");
    registerColumnType(String.class, 65535, "text($l)");
    registerColumnType(String.class, 16777215, "mediumtext($l)");
    registerColumnType(String.class, 4294967295L, "longtext($l)");
  }

  @Override
  public String getAddColumnString() {
    return "add column";
  }

  protected MySQLStorageEngine getDefaultMySQLStorageEngine() {
    return MyISAMStorageEngine.INSTANCE;
  }

  @Override
  public void addSupportedJavaTypes() {
    super.addSupportedJavaTypes();
    addSupportedJavaType(BitSet.class);
  }

  @Override
  public IdentityColumnSupport getIdentityColumnSupport() {
    return new MySQLIdentityColumnSupport();
  }

  @Override
  public String getTableTypeString() {
    return storageEngine.getTableTypeString(getEngineKeyword());
  }

  protected String getEngineKeyword() {
    return "type";
  }

  @Override
  public String getKeyHolderKey() {
    return "GENERATED_KEY";
  }
}
