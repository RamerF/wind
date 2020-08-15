package io.github.ramerf.wind.core.config;

import java.lang.reflect.Field;
import javax.persistence.Column;
import lombok.Data;

/**
 * 实体列信息(字段,sqlType).
 *
 * @author ramer
 * @since 13/08/2020
 */
@Data
public class EntityColumn {
  public static final int DEFAULT_LENGTH = 255;
  public static final int DEFAULT_PRECISION = 19;

  /** 列名. */
  private String name;

  /** 字段. */
  private Field field;

  /** 对应java类型. */
  private SqlType javaType;

  /** 长度. */
  private int length = DEFAULT_LENGTH;

  /** 精度,小数位数. */
  private int precision = DEFAULT_PRECISION;

  /** 是否可为空.true:可为空. */
  private boolean nullable = true;

  /** 是否唯一.true:唯一. */
  private boolean unique;

  /** 类型. */
  private SqlType sqlType;

  /** 备注. */
  private String comment;

  /** 默认值. */
  private String defaultValue;

  /** {@link Column#columnDefinition()}. */
  private String columnDefinition;

  /** 获取sql长度定义.如:(1,10)或(255). */
  public String getSqlLengthDefinition() {
    return sqlType.isContainPrecision() ? length + "," + precision : length + "";
  }

  public String getColumnDefinition() {
    StringBuilder definition = new StringBuilder();
    if (columnDefinition == null) {
      definition
          .append(name)
          .append(" ")
          .append(sqlType.getSqlType())
          .append("(")
          .append(getSqlLengthDefinition())
          .append(") ");
      if (!nullable) {
        definition.append("NOT NULL ");
      }
      if (defaultValue != null) {
        definition.append("DEFAULT ").append(defaultValue);
      }
    }
    return definition.toString();
  }

  /** 获取唯一定义sql. */
  public String getUniqueDefinition() {
    return unique ? String.format("CREATE UNIQUE INDEX %s_index ON TABLE(%s)", name, name) : null;
  }
}
