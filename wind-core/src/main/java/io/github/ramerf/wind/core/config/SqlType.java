package io.github.ramerf.wind.core.config;

import java.math.BigDecimal;
import java.sql.Timestamp;

/**
 * 数据库列定义类型.
 *
 * @author ramer
 * @since 14/08/2020
 */
public enum SqlType {
  /** bigint. */
  BIGINT("BIGINT", Long.class),
  BINARY("BINARY", null),
  BIT("BIT", Boolean.class),
  CHAR("CHAR", Character.class),
  DATETIME("DATETIME", null),
  DECIMAL("DECIMAL", BigDecimal.class),
  DOUBLE("DOUBLE", Double.class),
  FLOAT("FLOAT", Float.class),
  INT("INT", Integer.class),
  MONEY("MONEY", BigDecimal.class),
  NCHAR("NCHAR", Character.class),
  N_TEXT("N_TEXT", String.class),
  NUMERIC("NUMERIC", BigDecimal.class),
  NVARCHAR("NVARCHAR", String.class),
  SMALL_DATETIME("SMALL_DATETIME", Timestamp.class),
  SMALL_INT("SMALL_INT", Integer.class),
  SMALL_MONEY("SMALL_MONEY", BigDecimal.class),
  SQL_VARIANT("SQL_VARIANT", String.class),
  SYS_NAME("SYS_NAME", String.class),
  TEXT("TEXT", String.class),
  TIMESTAMP("TIMESTAMP", Timestamp.class),
  TINYINT("TINYINT", Integer.class),
  UNIQUE_IDENTIFIER("UNIQUE_IDENTIFIER", Character.class),
  VARBINARY("VARBINARY", null),
  VARCHAR("VARCHAR", String.class),
  ;
  private final String sqlType;
  private final Class<?> javaType;

  SqlType(final String sqlType, final Class<?> javaType) {
    this.sqlType = sqlType;
    this.javaType = javaType;
  }

  public String getSqlType() {
    return sqlType;
  }

  public Class<?> getJavaType() {
    return javaType;
  }

  /** 是否包含精度. */
  public boolean isContainPrecision() {
    return equals(FLOAT) || equals(DOUBLE) || equals(DECIMAL);
  }
}
