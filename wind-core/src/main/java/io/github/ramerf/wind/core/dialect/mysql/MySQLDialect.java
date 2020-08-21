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
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.util.Date;

/**
 * An SQL dialect for MySQL (prior to 5.x).
 *
 * @author Gavin King
 */
public class MySQLDialect extends Dialect {

  private final MySQLStorageEngine storageEngine;

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
    registerColumnType(String.class, 65535, "longtext");
  }

  @Override
  public String getAddColumnString() {
    return "add column";
  }

  protected MySQLStorageEngine getDefaultMySQLStorageEngine() {
    return MyISAMStorageEngine.INSTANCE;
  }

  @Override
  protected void addSupportedJavaType(final Type type) {
    super.addSupportedJavaType(type);
  }
}
