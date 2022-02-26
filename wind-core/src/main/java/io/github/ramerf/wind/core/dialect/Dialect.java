package io.github.ramerf.wind.core.dialect;

import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupport;
import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupportImpl;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.sql.*;
import java.time.*;
import java.util.Date;
import java.util.*;
import javax.sql.DataSource;
import lombok.extern.slf4j.Slf4j;

/** The type Dialect. */
@Slf4j
public abstract class Dialect {
  /** sql类型与长度映射. */
  private final TypeNames typeNames = new TypeNames();
  /** 支持的Java类型. */
  private final List<Type> supportedJavaTypes = new ArrayList<>();
  /** 不支持指定长度的sql类型.如: date,datetime */
  private final List<Type> notSupportedLengthTypes = new ArrayList<>();
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

    // 支持的Java类型
    addSupportedJavaTypes();
    // 不支持指定长度的sql类型
    addNotSupportedLengthTypes();
  }

  /**
   * Get an instance of the dialect specified by the current <tt>System</tt> properties.
   *
   * @return The specified Dialect
   * @throws WindException If no dialect was specified, or if it could not be instantiated.
   */
  public static Dialect getInstance(final String dialect) throws WindException {
    if (dialect != null) {
      return (Dialect) BeanUtils.initial(dialect);
    }
    return null;
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
      throw new WindException(e);
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
   * @throws WindException If no mapping was specified for that type.
   */
  public String getTypeName(Type type) throws WindException {
    final String result = typeNames.get(type);
    if (result == null) {
      throw new WindException("No default type mapping for (java.sql.Types) " + type);
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
   * @throws WindException If no mapping was specified for that type.
   */
  public String getTypeName(Type type, long length, int precision, int scale)
      throws WindException {
    if (!isSupportJavaType(type)) {
      throw new WindException("Not supported type " + type.getTypeName());
    }
    final String result =
        notSupportedLengthType(type)
            ? typeNames.get(type)
            : typeNames.get(type, length, precision, scale);
    if (result == null) {
      throw new WindException(
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

  public final boolean isSupportJavaType(final Type type) {
    return supportedJavaTypes.contains(type);
  }

  protected void addSupportedJavaType(final Type type) {
    supportedJavaTypes.add(type);
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

    addSupportedJavaType(Date.class);
    addSupportedJavaType(LocalDate.class);
    addSupportedJavaType(LocalTime.class);
    addSupportedJavaType(LocalDateTime.class);
  }

  public void addNotSupportedLengthTypes() {
    notSupportedLengthTypes.add(Date.class);

    notSupportedLengthTypes.add(LocalDate.class);
    notSupportedLengthTypes.add(LocalTime.class);
    notSupportedLengthTypes.add(LocalDateTime.class);
  }

  /** 是否支持指定长度. */
  public final boolean notSupportedLengthType(final Type type) {
    return notSupportedLengthTypes.contains(type);
  }

  public String getCommonOnTableString(
      String category, String schema, String table, String comment) {
    throw new WindException("Not implemented");
  }

  public String getCommentOnColumnString(
      final String table, final String column, final String comment) {
    throw new RuntimeException("Not implemented");
  }

  public boolean isSupportCommentOn() {
    return false;
  }

  /**
   * Get the appropriate {@link IdentityColumnSupport}
   *
   * @return the IdentityColumnSupport
   * @since 5.1
   */
  public IdentityColumnSupport getIdentityColumnSupport() {
    return new IdentityColumnSupportImpl();
  }

  public String getTableTypeString() {
    // grrr... for differentiation of mysql storage engines
    return "";
  }

  public abstract String getKeyHolderKey();
}
