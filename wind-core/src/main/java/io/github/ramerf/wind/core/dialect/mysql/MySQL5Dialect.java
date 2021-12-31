/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.mysql;

/**
 * An SQL dialect for MySQL 5.x specific features.
 *
 * @author Steve Ebersole
 */
public class MySQL5Dialect extends MySQLDialect {
  public MySQL5Dialect() {
    // registerColumnType(String.class, "longtext");
    //		registerColumnType( Types.VARCHAR, 16777215, "mediumtext" );
    // registerColumnType(String.class, 65535, "varchar($l)");
  }

  @Override
  protected String getEngineKeyword() {
    return "engine";
  }
}
