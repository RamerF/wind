package io.github.ramerf.wind.core.config;

import java.lang.reflect.Field;

/**
 * 实体列信息(字段,sqlType).
 *
 * @author ramer
 * @since 13/08/2020
 */
public class EntityColumn {
  public static final int DEFAULT_LENGTH = 255;
  public static final int DEFAULT_PRECISION = 19;
  /** 列名. */
  private String name;
  /** 列名. */
  private Field field;
  /** 列名. */
  private String javaType;
  /** 列名. */
  private int length = DEFAULT_LENGTH;
  /** 列名. */
  private int precision = DEFAULT_PRECISION;
  /** 列名. */
  private boolean nullable = true;
  /** 列名. */
  private boolean unique;
  /** 列名. */
  private String sqlType;
  /** 列名. */
  private boolean quoted;
  /** 列名. */
  private String comment;
  /** 列名. */
  private String defaultValue;
}
