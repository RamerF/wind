package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.sql.*;
import java.time.*;
import java.util.*;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/** The type Dialect. */
@Slf4j
public abstract class Dialect {
  private final TypeNames typeNames = new TypeNames();
  private final List<Type> supportedDateTypes = new ArrayList<>();
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

  protected DataSource dataSource;

  public void setDataSource(final DataSource dataSource) {
    this.dataSource = dataSource;
  }

  /** Instantiates a new Dialect. */
  protected Dialect() {
    // char type
    registerColumnType(char.class, "char($l)");
    registerColumnType(Character.class, "char($l)");
    // boolean type
    registerColumnType(boolean.class, "bit varying($l)");
    registerColumnType(Boolean.class, "bit varying($l)");
    // value type
    registerColumnType(Byte.class, "boolean");
    registerColumnType(byte.class, "boolean");

    registerColumnType(Short.class, "tinyint");
    registerColumnType(short.class, "tinyint");

    registerColumnType(Integer.class, 3, "tinyint");
    registerColumnType(int.class, 3, "tinyint");

    registerColumnType(Integer.class, "smallint");
    registerColumnType(int.class, "smallint");

    registerColumnType(Integer.class, 8, "integer");
    registerColumnType(int.class, 8, "integer");

    registerColumnType(Long.class, "bigint");
    registerColumnType(long.class, "bigint");

    registerColumnType(Float.class, "float($p)");
    registerColumnType(float.class, "float($p)");

    registerColumnType(Double.class, "double precision");
    registerColumnType(double.class, "double precision");

    registerColumnType(BigDecimal.class, "numeric($p,$s)");
    // date type
    registerColumnType(LocalDate.class, "date");
    registerColumnType(LocalTime.class, "time");
    registerColumnType(LocalDateTime.class, "timestamp");
    // varchar type
    registerColumnType(String.class, "varchar($l)");

    addSupportedJavaTypes();
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

  /** 通过数据库元数据获取方言. */
  public static Dialect getInstance(DataSource dataSource) {
    Connection connection;
    final DatabaseMetaData databaseMetaData;
    try {
      connection = dataSource.getConnection();
      databaseMetaData = connection.getMetaData();
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      throw CommonException.of(e);
    }

    for (DatabaseEnum database : DatabaseEnum.values()) {
      Dialect dialect = database.resolveDialect(databaseMetaData);
      if (dialect != null) {
        dialect.setDataSource(dataSource);
        return dialect;
      }
    }
    throw new IllegalStateException("can not initial dialect, check data source.");
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

  /**
   * Get the name of the database type associated with the given {@link java.sql.Types} typecode.
   *
   * @param type The type key
   * @return the database type name
   * @throws CommonException If no mapping was specified for that type.
   */
  public String getTypeName(Type type) throws CommonException {
    final String result = typeNames.get(type);
    if (result == null) {
      throw CommonException.of("No default type mapping for (java.sql.Types) " + type);
    }
    return result;
  }

  /**
   * Get the name of the database type associated with the given {@link java.sql.Types} typecode
   * with the given storage specification parameters.
   *
   * @param type The java type
   * @param length The datatype length
   * @param precision The datatype precision
   * @param scale The datatype scale
   * @return the database type name
   * @throws CommonException If no mapping was specified for that type.
   */
  public String getTypeName(Type type, long length, int precision, int scale)
      throws CommonException {
    if (!isSupportJavaType(type)) {
      throw CommonException.of("Not supported type " + type.getTypeName());
    }
    final String result = typeNames.get(type, length, precision, scale);
    if (result == null) {
      throw CommonException.of(
          String.format("No sql type mapping for java type: %s, length: %s", type, length));
    }
    return result;
  }

  /**
   * Subclasses register a type name for the given type clazz and maximum column length. <tt>$l</tt>
   * in the type name with be replaced by the column length (if appropriate).
   *
   * @param type The java type
   * @param capacity The maximum length of database type
   * @param name The database type name
   */
  protected void registerColumnType(Type type, long capacity, String name) {
    typeNames.put(type, capacity, name);
  }

  /**
   * Subclasses register a type name for the given type clazz. <tt>$l</tt> in the type name with be
   * replaced by the column length (if appropriate).
   *
   * @param type The java type
   * @param name The database type name
   */
  protected void registerColumnType(Type type, String name) {
    typeNames.put(type, name);
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

  public boolean isSupportJavaType(final Type type) {
    return supportedDateTypes.contains(type);
  }

  protected void addSupportedJavaType(final Type type) {
    supportedDateTypes.add(type);
  }

  public void addSupportedJavaTypes() {
    addSupportedJavaType(char.class);
    addSupportedJavaType(Character.class);

    addSupportedJavaType(boolean.class);
    addSupportedJavaType(Boolean.class);

    addSupportedJavaType(byte.class);
    addSupportedJavaType(Byte.class);
    addSupportedJavaType(short.class);
    addSupportedJavaType(Short.class);
    addSupportedJavaType(int.class);
    addSupportedJavaType(Integer.class);
    addSupportedJavaType(long.class);
    addSupportedJavaType(Long.class);
    addSupportedJavaType(float.class);
    addSupportedJavaType(Float.class);
    addSupportedJavaType(double.class);
    addSupportedJavaType(Double.class);
    addSupportedJavaType(BigDecimal.class);
    addSupportedJavaType(String.class);
  }
}
