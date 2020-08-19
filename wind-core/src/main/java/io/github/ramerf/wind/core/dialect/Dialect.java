package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.sql.*;
import java.util.*;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/** The type Dialect. */
@Slf4j
public abstract class Dialect {
  private final TypeNames typeNames = new TypeNames();
  /** Defines a default batch size constant */
  public static final String DEFAULT_BATCH_SIZE = "15";

  /** Defines a "no batching" batch size constant */
  public static final String NO_BATCH = "0";

  /** Characters used as opening for quoting SQL identifiers */
  public static final String QUOTE = "`\"[";

  /** Characters used as closing for quoting SQL identifiers */
  public static final String CLOSED_QUOTE = "`\"]";

  private final Properties properties = new Properties();
  private final Set<String> sqlKeywords = new HashSet<>();

  /** Instantiates a new Dialect. */
  protected Dialect() {
    registerColumnType(Types.BIT, "bit");
    registerColumnType(Types.BOOLEAN, "boolean");
    registerColumnType(Types.TINYINT, "tinyint");
    registerColumnType(Types.SMALLINT, "smallint");
    registerColumnType(Types.INTEGER, "integer");
    registerColumnType(Types.BIGINT, "bigint");
    registerColumnType(Types.FLOAT, "float($p)");
    registerColumnType(Types.DOUBLE, "double precision");
    registerColumnType(Types.NUMERIC, "numeric($p,$s)");
    registerColumnType(Types.REAL, "real");

    registerColumnType(Types.DATE, "date");
    registerColumnType(Types.TIME, "time");
    registerColumnType(Types.TIMESTAMP, "timestamp");

    registerColumnType(Types.VARBINARY, "bit varying($l)");
    registerColumnType(Types.LONGVARBINARY, "bit varying($l)");
    registerColumnType(Types.BLOB, "blob");

    registerColumnType(Types.CHAR, "char($l)");
    registerColumnType(Types.VARCHAR, "varchar($l)");
    registerColumnType(Types.LONGVARCHAR, "varchar($l)");
    registerColumnType(Types.CLOB, "clob");

    registerColumnType(Types.NCHAR, "nchar($l)");
    registerColumnType(Types.NVARCHAR, "nvarchar($l)");
    registerColumnType(Types.LONGNVARCHAR, "nvarchar($l)");
    registerColumnType(Types.NCLOB, "nclob");
  }

  /**
   * Get an instance of the dialect specified by the current <tt>System</tt> properties.
   *
   * @return The specified Dialect
   * @throws CommonException If no dialect was specified, or if it could not be instantiated.
   */
  public static Dialect getDialect() throws CommonException {
    return instantiateDialect(AppContextInject.getBean(WindConfiguration.class).getDialect());
  }

  /**
   * Get an instance of the dialect specified by the given properties or by the current
   * <tt>System</tt> properties.
   *
   * @param props The properties to use for finding the dialect class to use.
   * @return The specified Dialect
   * @throws CommonException If no dialect was specified, or if it could not be instantiated.
   */
  public static Dialect getDialect(Properties props) throws CommonException {
    final String dialectName = props.getProperty("wind.dialect");
    if (dialectName == null) {
      return getDialect();
    }
    return instantiateDialect(dialectName);
  }

  private static Dialect instantiateDialect(String dialectName) throws CommonException {
    if (dialectName == null) {
      throw CommonException.of("The dialect was not set. Set the property [wind.dialect].");
    }
    return (Dialect) BeanUtils.initial(dialectName);
  }

  /**
   * Retrieve a set of default Hibernate properties for this database.
   *
   * @return a set of Hibernate properties
   */
  public final Properties getDefaultProperties() {
    return properties;
  }

  @Override
  public String toString() {
    return getClass().getName();
  }

  /** 获取所有的表信息. */
  public abstract List<String> getTables(DataSource dataSource);

  /**
   * Get the name of the database type associated with the given {@link java.sql.Types} typecode.
   *
   * @param code The {@link java.sql.Types} typecode
   * @return the database type name
   * @throws CommonException If no mapping was specified for that type.
   */
  public String getTypeName(int code) throws CommonException {
    final String result = typeNames.get(code);
    if (result == null) {
      throw CommonException.of("No default type mapping for (java.sql.Types) " + code);
    }
    return result;
  }

  /**
   * Get the name of the database type associated with the given {@link java.sql.Types} typecode
   * with the given storage specification parameters.
   *
   * @param code The {@link java.sql.Types} typecode
   * @param length The datatype length
   * @param precision The datatype precision
   * @param scale The datatype scale
   * @return the database type name
   * @throws CommonException If no mapping was specified for that type.
   */
  public String getTypeName(int code, long length, int precision, int scale)
      throws CommonException {
    final String result = typeNames.get(code, length, precision, scale);
    if (result == null) {
      throw CommonException.of(
          String.format("No type mapping for java.sql.Types code: %s, length: %s", code, length));
    }
    return result;
  }

  /**
   * Subclasses register a type name for the given type code and maximum column length. <tt>$l</tt>
   * in the type name with be replaced by the column length (if appropriate).
   *
   * @param code The {@link java.sql.Types} typecode
   * @param capacity The maximum length of database type
   * @param name The database type name
   */
  protected void registerColumnType(int code, long capacity, String name) {
    typeNames.put(code, capacity, name);
  }

  /**
   * Subclasses register a type name for the given type code. <tt>$l</tt> in the type name with be
   * replaced by the column length (if appropriate).
   *
   * @param code The {@link java.sql.Types} typecode
   * @param name The database type name
   */
  protected void registerColumnType(int code, String name) {
    typeNames.put(code, name);
  }

  /**
   * The syntax used to add a column to a table (optional).
   *
   * @return The "add column" fragment.
   */
  public String getAddColumnString() {
    throw new UnsupportedOperationException(
        "No add column syntax supported by " + getClass().getName());
  }

  /**
   * The syntax for the suffix used to add a column to a table (optional).
   *
   * @return The suffix "add column" fragment.
   */
  public String getAddColumnSuffixString() {
    return "";
  }
}
