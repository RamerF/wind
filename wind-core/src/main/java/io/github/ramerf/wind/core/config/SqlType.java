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
  BIGINT(Long.class, "BIGINT"),
  BINARY(null, "BINARY"),
  BIT(Boolean.class, "BIT"),
  CHAR(Character.class, "CHAR"),
  DATETIME(null, "DATETIME"),
  DECIMAL(BigDecimal.class, "DECIMAL"),
  DOUBLE(Double.class, "DOUBLE"),
  FLOAT(Float.class, "FLOAT"),
  INT(Integer.class, "INT"),
  MONEY(BigDecimal.class, "MONEY"),
  NCHAR(Character.class, "NCHAR"),
  N_TEXT(String.class, "N_TEXT"),
  NUMERIC(BigDecimal.class, "NUMERIC"),
  NVARCHAR(String.class, "NVARCHAR"),
  SMALL_DATETIME(Timestamp.class, "SMALL_DATETIME"),
  SMALL_INT(Integer.class, "SMALL_INT"),
  SMALL_MONEY(BigDecimal.class, "SMALL_MONEY"),
  SQL_VARIANT(String.class, "SQL_VARIANT"),
  SYS_NAME(String.class, "SYS_NAME"),
  TEXT(String.class, "TEXT"),
  TIMESTAMP(Timestamp.class, "TIMESTAMP"),
  TINYINT(Integer.class, "TINYINT"),
  UNIQUE_IDENTIFIER(Character.class, "UNIQUE_IDENTIFIER"),
  VARBINARY(null, "VARBINARY"),
  VARCHAR(String.class, "VARCHAR"),
  ;
  private final String sqlType;
  private final Class<?> javaType;

  SqlType(final Class<?> javaType, final String sqlType) {
    this.javaType = javaType;
    this.sqlType = sqlType;
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
