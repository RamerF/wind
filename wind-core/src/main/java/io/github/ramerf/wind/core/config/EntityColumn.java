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
  /** 长度. */
  private int length = DEFAULT_LENGTH;
  /** 精度,小数位数. */
  private int precision = DEFAULT_PRECISION;
  /** 是否可为空.true:可为空. */
  private boolean nullable = true;
  /** 是否唯一.true:唯一. */
  private boolean unique;
  /** 类型. */
  private String sqlType;
  /** 备注. */
  private String comment;
  /** 默认值. */
  private String defaultValue;
}
