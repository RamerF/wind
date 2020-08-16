/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.mysql;

import java.sql.Types;

/**
 * An SQL dialect for MySQL 5.x specific features.
 *
 * @author Steve Ebersole
 */
public class MySQL5Dialect extends MySQLDialect {
  @Override
  protected void registerVarcharTypes() {
    registerColumnType(Types.VARCHAR, "longtext");
    //		registerColumnType( Types.VARCHAR, 16777215, "mediumtext" );
    registerColumnType(Types.VARCHAR, 65535, "varchar($l)");
    registerColumnType(Types.LONGVARCHAR, "longtext");
  }
}
