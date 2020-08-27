package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableColumn;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import javax.annotation.Nonnull;
import javax.persistence.Column;
import javax.persistence.Id;
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

  /** 是否是主键. */
  private boolean primaryKey = false;

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
    definition.append(name).append(" ").append(typeName);
    if (!nullable) {
      definition.append(" not null");
    }
    if (defaultValue != null) {
      definition.append(" default ").append(defaultValue);
    }
    return definition.toString();
  }

  public String getComment(final String tableName, final Dialect dialect) {
    return dialect.getCommonOnColumnString(tableName, name, comment);
  }

  /** 获取唯一定义sql. */
  public String getUniqueDefinition() {
    return unique ? String.format("create unique index %s_index on table(%s)", name, name) : null;
  }

  public static EntityColumn of(@Nonnull Field field, Dialect dialect) {
    EntityColumn entityColumn = new EntityColumn();
    entityColumn.field = field;
    entityColumn.name = EntityUtils.fieldToColumn(field);
    entityColumn.type = field.getGenericType();
    if (dialect.isSupportJavaType(entityColumn.type)
        || (entityColumn.type instanceof Class
            && InterEnum.class.isAssignableFrom((Class<?>) entityColumn.type))) {
      entityColumn.supported = true;
    }
    if (field.isAnnotationPresent(Id.class)) {
      entityColumn.primaryKey = true;
    }

    final TableColumn tableColumn = field.getAnnotation(TableColumn.class);
    if (tableColumn != null) {
      entityColumn.comment = tableColumn.comment();
      entityColumn.defaultValue = tableColumn.defaultValue();
    }

    // 如果是基本类型,列定义不能为空
    if (entityColumn.getType() instanceof Class
        && BeanUtils.isPrimitiveType((Class<?>) entityColumn.getType())) {
      entityColumn.nullable = false;
    }

    final Column column = field.getAnnotation(Column.class);
    if (column == null) {
      entityColumn.typeName =
          entityColumn.supported ? dialect.getTypeName(entityColumn.type) : null;

      // 默认主键定义
      if (entityColumn.isPrimaryKey()) {
        entityColumn.columnDefinition = getPrimaryKeyDefinition(dialect, entityColumn);
      }
      return entityColumn;
    }

    StringUtils.doIfNonEmpty(column.name(), name -> entityColumn.name = name);
    StringUtils.doIfNonEmpty(
        column.columnDefinition(),
        columnDefinition -> {
          final String upperCaseDefinition = columnDefinition.toUpperCase();
          final int start = upperCaseDefinition.indexOf("DEFAULT");
          if (start == -1
              || tableColumn == null
              || StringUtils.isEmpty(entityColumn.defaultValue)) {
            entityColumn.columnDefinition = columnDefinition;
            return;
          }
          // TableColumn#defaultValue优先级高于Column#columnDefinition中的default值
          entityColumn.columnDefinition =
              columnDefinition.substring(0, start + 8) + entityColumn.defaultValue;
        });

    entityColumn.length = column.length();
    // 使用默认值而不是0
    NumberUtils.doIfGreaterThanZero(column.precision(), o -> entityColumn.precision = o);
    NumberUtils.doIfGreaterThanZero(column.scale(), o -> entityColumn.scale = o);

    entityColumn.nullable = entityColumn.nullable && column.nullable();
    entityColumn.unique = column.unique();

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

  private static String getPrimaryKeyDefinition(
      final Dialect dialect, final EntityColumn entityColumn) {
    final IdGenerator idGenerator = AppContextInject.getBean(IdGenerator.class);
    Long id = 1L;
    try {
      id = idGenerator.nextId(null);
    } catch (Exception ignore) {
      // 用户自定义的id生成器可能会用到obj参数,此时会抛异常,认为主键非自增
    }
    // 只有id为null时才拼接自增定义
    return id == null
        ? dialect.getIdentityColumnSupport().getIdentityColumnString(entityColumn.type)
        : entityColumn.columnDefinition;
  }
}
