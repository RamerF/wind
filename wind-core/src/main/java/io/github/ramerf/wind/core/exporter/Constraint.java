/*
 * Hibernate, Relational Persistence for Idiomatic Java
 *
 * License: GNU Lesser General Public License (LGPL), version 2.1 or later.
 * See the lgpl.txt file in the root directory or <http://www.gnu.org/licenses/lgpl-2.1.html>.
 */
package io.github.ramerf.wind.core.exporter;

import io.github.ramerf.wind.core.config.EntityColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.StringUtils;
import java.io.Serializable;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import lombok.Getter;

/**
 * A relational constraint.
 *
 * @author Gavin King
 * @author Brett Meyer
 */
public abstract class Constraint implements Serializable {

  private String name;
  private final List<EntityColumn> columns = new ArrayList<>();
  @Getter private EntityInfo table;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  /**
   * If a constraint is not explicitly named, this is called to generate a unique hash using the
   * table and column names. Static so the name can be generated prior to creating the Constraint.
   * They're cached, keyed by name, in multiple locations.
   *
   * @return String The generated name
   */
  public static String generateName(String prefix, EntityInfo table, EntityColumn... columns) {
    // Use a concatenation that guarantees uniqueness, even if identical names
    // exist between all table and column identifiers.

    StringBuilder sb = new StringBuilder("table`" + table.getName() + "`");

    // Ensure a consistent ordering of columns, regardless of the order
    // they were bound.
    // Clone the list, as sometimes a set of order-dependent Column
    // bindings are given.
    EntityColumn[] alphabeticalColumns = columns.clone();
    for (EntityColumn column : alphabeticalColumns) {
      String columnName = column == null ? "" : column.getName();
      sb.append("column`").append(columnName).append("`");
    }
    return prefix + hashedName(sb.toString());
  }

  /**
   * Helper method for {@link #generateName(String, EntityInfo, EntityColumn...)}.
   *
   * @return String The generated name
   */
  public static String generateName(String prefix, EntityInfo table, List<EntityColumn> columns) {
    return generateName(prefix, table, columns.toArray(new EntityColumn[0]));
  }

  /**
   * Hash a constraint name using MD5. Convert the MD5 digest to base 35 (full alphanumeric),
   * guaranteeing that the length of the name will always be smaller than the 30 character
   * identifier restriction enforced by a few dialects.
   *
   * @param s The name to be hashed.
   * @return String The hased name.
   */
  public static String hashedName(String s) {
    try {
      MessageDigest md = MessageDigest.getInstance("MD5");
      md.reset();
      md.update(s.getBytes());
      byte[] digest = md.digest();
      BigInteger bigInt = new BigInteger(1, digest);
      // By converting to base 35 (full alphanumeric), we guarantee
      // that the length of the name will always be smaller than the 30
      // character identifier restriction enforced by a few dialects.
      return bigInt.toString(35);
    } catch (NoSuchAlgorithmException e) {
      throw CommonException.of("Unable to generate a hashed Constraint name!", e);
    }
  }

  public void addColumn(EntityColumn column) {
    if (!columns.contains(column)) {
      columns.add(column);
    }
  }

  /** @return true if this constraint already contains a column with same name. */
  public boolean containsColumn(EntityColumn column) {
    return columns.contains(column);
  }

  public boolean isGenerated(Dialect dialect) {
    return true;
  }

  public String sqlDropString(Dialect dialect, String defaultCatalog, String defaultSchema) {
    if (isGenerated(dialect)) {
      final String tableName = getTable().getName();
      return String.format(
          Locale.ROOT, "%s drop constraint `%s`", "alter table ".concat(tableName), getName());
    } else {
      return null;
    }
  }

  public String sqlCreateString(Dialect dialect, String defaultCatalog, String defaultSchema) {
    if (isGenerated(dialect)) {
      // Certain dialects (ex: HANA) don't support FKs as expected, but other constraints can still
      // be created.
      // If that's the case, hasAlterTable() will be true, but getAddForeignKeyConstraintString will
      // return
      // empty string.  Prevent blank "alter table" statements.
      String constraintString =
          sqlConstraintString(dialect, getName(), defaultCatalog, defaultSchema);
      if (StringUtils.nonEmpty(constraintString)) {
        final String tableName = getTable().getName();
        return "alter table " + getTable().getName() + " " + constraintString;
      }
    }
    return null;
  }

  public abstract String sqlConstraintString(
      Dialect d, String constraintName, String defaultCatalog, String defaultSchema);

  /** 约束的前缀. 如: "UK_", "FK_", and "PK_". */
  public abstract String prefix();
}
