/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.postgresql;

import io.github.ramerf.wind.core.dialect.Dialect;
import java.sql.*;
import java.util.*;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/**
 * An SQL dialect for Postgres
 *
 * <p>For discussion of BLOB support in Postgres, as of 8.4, have a peek at <a
 * href="http://jdbc.postgresql.org/documentation/84/binary-data.html">http://jdbc.postgresql.org/documentation/84/binary-data.html</a>.
 * For the effects in regards to Hibernate see <a
 * href="http://in.relation.to/15492.lace">http://in.relation.to/15492.lace</a>
 *
 * @author Gavin King
 */
@Slf4j
public class PostgreSQL81Dialect extends Dialect {

  @Override
  public List<String> getTables(DataSource dataSource) {
    final Connection connection;
    try {
      connection = dataSource.getConnection();
      final DatabaseMetaData databaseMetaData = connection.getMetaData();
      final ResultSet resultSet =
          databaseMetaData.getTables(
              connection.getCatalog(), connection.getSchema(), "%%", new String[] {"TABLE"});
      List<String> tables = new ArrayList<>();
      while (resultSet.next()) {
        tables.add(resultSet.getString(3));
      }
      return tables;
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
    }
    return Collections.emptyList();
  }
}
