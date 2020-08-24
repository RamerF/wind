package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import lombok.*;

/**
 * 实体列信息(字段,sqlType).
 *
 * @author ramer
 * @since 13/08/2020
 */
@Data
public class EntityColumn {
  /** 非浮点数长度. */
  public static final int DEFAULT_LENGTH = 255;
  /** 浮点数长度. */
  public static final int DEFAULT_PRECISION = 19;
  /** 小数位数. */
  public static final int DEFAULT_SCALE = 2;

  /** 列名. */
  private String name;

  /** 字段. */
  private Field field;

  /** 对应java类型. */
  private Type type;

  private String typeName;

  /** 长度. */
  private int length = DEFAULT_LENGTH;

  /** Numeric类型长度. */
  private int precision = DEFAULT_PRECISION;

  /** 精度,小数点位数. */
  private int scale = DEFAULT_SCALE;

  /** 是否可为空.true:可为空. */
  private boolean nullable = true;

  /** 是否唯一.true:唯一. */
  private boolean unique = false;

  /** 备注. */
  private String comment;

  /** 默认值. */
  private String defaultValue;

  /** {@link Column#columnDefinition()}. */
  @Getter(AccessLevel.NONE)
  private String columnDefinition;

  /** 数据库是否支持列类型. 如果不支持,ddl时会跳过该字段. */
  private boolean supported = false;

  public String getColumnDefinition(final Dialect dialect) {
    if (columnDefinition != null) {
      return name + " " + columnDefinition;
    }
    StringBuilder definition = new StringBuilder();
    definition
        .append(name)
        .append(" ")
        .append(supported ? dialect.getTypeName(getType(), length, precision, scale) : "");
    if (!nullable) {
      definition.append(" NOT NULL");
    }
    if (defaultValue != null) {
      definition.append(" DEFAULT ").append(defaultValue);
    }
    return definition.toString();
  }

  public String getComment(final String tableName, final Dialect dialect) {
    return dialect.getCommonOnColumnString(tableName, name, comment);
  }

  /** 获取唯一定义sql. */
  public String getUniqueDefinition() {
    return unique ? String.format("CREATE UNIQUE INDEX %s_index ON TABLE(%s)", name, name) : null;
  }

  public static EntityColumn of(@Nonnull Field field, Dialect dialect) {
    EntityColumn entityColumn = new EntityColumn();
    entityColumn.field = field;
    entityColumn.name = EntityUtils.fieldToColumn(field);
    entityColumn.type = field.getGenericType();
    if (dialect.isSupportJavaType(entityColumn.type)) {
      entityColumn.supported = true;
    }
    final Column column = field.getAnnotation(Column.class);
    if (column == null) {
      // SqlType  根据范围决定使用的sql类型,如: 0-255 varchar, 255-65535 text, 255-65535 mediumtext, 65535-n
      // longtext
      // TODO: 继续跟踪 StandardBasicTypes 里面的类型对应了数据库类型,暂时可以简单的用属性的类型对应数据库类型
      // entityColumn.sqlType = null;
      entityColumn.typeName =
          entityColumn.supported ? dialect.getTypeName(entityColumn.type) : null;
      return entityColumn;
    }

    StringUtils.doIfNonEmpty(column.name(), name -> entityColumn.name = name);
    StringUtils.doIfNonEmpty(
        column.columnDefinition(),
        columnDefinition -> entityColumn.columnDefinition = columnDefinition);

    entityColumn.length = column.length();
    // 使用默认值而不是0
    NumberUtils.doIfGreaterThanZero(column.precision(), o -> entityColumn.precision = o);
    NumberUtils.doIfGreaterThanZero(column.scale(), o -> entityColumn.scale = o);

    entityColumn.nullable = column.nullable();
    entityColumn.unique = column.unique();

    final TableColumn tableColumn = field.getAnnotation(TableColumn.class);
    if (tableColumn != null) {
      entityColumn.comment = tableColumn.comment();
    }

    if (entityColumn.columnDefinition == null) {
      entityColumn.typeName =
          entityColumn.supported
              ? dialect.getTypeName(
                  entityColumn.type,
                  entityColumn.length,
                  entityColumn.precision,
                  entityColumn.scale)
              : null;
    }
    return entityColumn;
  }
}