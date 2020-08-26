/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.dialect.postgresql;

import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupport;
import io.github.ramerf.wind.core.dialect.identity.PostgreSQL81IdentityColumnSupport;
import io.github.ramerf.wind.core.type.JavaType;
import java.util.BitSet;
import java.util.Date;
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
  public PostgreSQL81Dialect() {
    super();
    // boolean type
    registerColumnType(boolean.class, "bool");
    registerColumnType(Boolean.class, "bool");

    registerColumnType(String.class, "text");
    registerColumnType(String.class, 255, "text");
    registerColumnType(String.class, 65535, "text");

    registerColumnType(Date.class, "timestamp");
    // array
    registerColumnType(int[].class, "int[]");
    registerColumnType(Integer[].class, "int[]");
    registerColumnType(JavaType.LIST_INTEGER, "int[]");

    registerColumnType(long[].class, "bigint[]");
    registerColumnType(Long[].class, "bigint[]");
    registerColumnType(JavaType.LIST_LONG, "bigint[]");

    registerColumnType(String[].class, "text[]");
    registerColumnType(JavaType.LIST_STRING, "text[]");
  }

  @Override
  public String getAddColumnString() {
    return "add column";
  }

  @Override
  public void addSupportedJavaTypes() {
    super.addSupportedJavaTypes();

    addSupportedJavaType(char[].class);
    addSupportedJavaType(Character[].class);

    addSupportedJavaType(short[].class);
    addSupportedJavaType(Short[].class);
    addSupportedJavaType(int[].class);
    addSupportedJavaType(Integer[].class);
    addSupportedJavaType(long[].class);
    addSupportedJavaType(Long[].class);

    addSupportedJavaType(String[].class);

    addSupportedJavaType(JavaType.LIST_SHORT);
    addSupportedJavaType(JavaType.LIST_INTEGER);
    addSupportedJavaType(JavaType.LIST_LONG);
    addSupportedJavaType(JavaType.LIST_FLOAT);
    addSupportedJavaType(JavaType.LIST_DOUBLE);
    addSupportedJavaType(JavaType.LIST_BIGDECIMAL);

    addSupportedJavaType(JavaType.LIST_STRING);

    addSupportedJavaType(BitSet.class);
  }

  @Override
  public String getCommonOnTableString(
      final String category, final String schema, final String table, final String comment) {
    return String.format("comment on table %s is '%s'", table, comment);
  }

  @Override
  public String getCommonOnColumnString(
      final String table, final String column, final String comment) {
    return String.format("comment on column %s.%s is '%s'", table, column, comment);
  }

  @Override
  public boolean isSupportCommentOn() {
    return true;
  }

  @Override
  public IdentityColumnSupport getIdentityColumnSupport() {
    return new PostgreSQL81IdentityColumnSupport();
  }
}
