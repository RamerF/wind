package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.dialect.identity.IdentityColumnSupport;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.*;
import java.util.Objects;
import javax.annotation.Nonnull;
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

  /** sql类型名. */
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

  /** {@link TableColumn#columnDefinition()}. */
  @Getter(AccessLevel.NONE)
  private String columnDefinition;

  /** 数据库是否支持列类型. 如果不支持,ddl时会跳过该字段. */
  private boolean supported = false;

  public String getColumnDdl(final Dialect dialect) {
    return name + " " + columnDefinition;
  }

  public String getComment(final String tableName, final Dialect dialect) {
    return dialect.getCommentOnColumnString(tableName, name, comment);
  }

  /** 获取唯一定义sql. */
  public String getUniqueDefinition() {
    return unique ? String.format("create unique index %s_index on table(%s)", name, name) : null;
  }

  private static final String defaultRegex = "default[ ]+[']?[\\w\\u4e00-\\u9fa5]+[']?[ ]?";
  private static final String commentRegex = "comment.*";

  public static EntityColumn of(@Nonnull Field field, Dialect dialect) {
    // 如果是一对多关联的情况,不记录列信息
    if (MappingInfo.isManyMapping(field)) {
      return null;
    }
    EntityColumn entityColumn = new EntityColumn();
    entityColumn.field = field;
    entityColumn.name = EntityUtils.fieldToColumn(field);

    final boolean oneMapping = MappingInfo.isOneMapping(field);
    final Field parseField = oneMapping ? getReferenceField(field) : field;
    // OneToOne时,可能不会添加列
    if (oneMapping) {
      final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
      if (oneToOne != null && !oneToOne.shouldJoinColumn()) {
        entityColumn.supported = false;
      }
    }
    entityColumn.type = parseField.getGenericType();

    final TableColumn tableColumn = parseField.getAnnotation(TableColumn.class);
    if (dialect.isSupportJavaType(entityColumn.type)
        || (entityColumn.type instanceof Class
            && InterEnum.class.isAssignableFrom((Class<?>) entityColumn.type))
        || tableColumn != null) {
      entityColumn.supported = true;
    }
    if (field.isAnnotationPresent(Id.class)) {
      entityColumn.primaryKey = true;
    }
    // 如果是基本类型,列定义不能为空
    if (entityColumn.getType() instanceof Class
        && ((Class<?>) entityColumn.getType()).isPrimitive()) {
      entityColumn.nullable = false;
    }
    if (tableColumn == null) {
      entityColumn.typeName =
          entityColumn.supported
              ? dialect.getTypeName(
                  entityColumn.getType(field, entityColumn.type),
                  entityColumn.length,
                  entityColumn.precision,
                  entityColumn.scale)
              : null;

    } else {
      entityColumn.comment = tableColumn.comment();
      if (!tableColumn.defaultValue().isEmpty()) {
        entityColumn.defaultValue = tableColumn.defaultValue();
      } else if (tableColumn.defaultBlankValue()) {
        entityColumn.defaultValue = "''";
      }
      // StringUtils.doIfNonEmpty(tableColumn.name(), name -> entityColumn.name = name);
      StringUtils.doIfNonEmpty(
          tableColumn.columnDefinition(),
          columnDefinition -> {
            columnDefinition = columnDefinition.toLowerCase();
            entityColumn.columnDefinition = columnDefinition;
            String defaultValue = entityColumn.defaultValue;
            if (StringUtils.nonEmpty(defaultValue)) {
              String replacement =
                  " default '" + (defaultValue.equals("''") ? "" : defaultValue) + "'";
              entityColumn.columnDefinition =
                  columnDefinition.replaceAll(defaultRegex, replacement);
              if (!entityColumn.columnDefinition.contains("default")) {
                entityColumn.columnDefinition += replacement;
              }
            }
            String comment = entityColumn.comment;
            if (StringUtils.nonEmpty(comment)) {
              String replacement = " comment '" + comment + "'";
              entityColumn.columnDefinition =
                  columnDefinition.replaceAll(commentRegex, replacement);
              if (!dialect.isSupportCommentOn()
                  && !entityColumn.columnDefinition.contains("comment")) {
                entityColumn.columnDefinition += replacement;
              }
            }
          });

      entityColumn.length = tableColumn.length();
      // 使用默认值而不是0
      NumberUtils.doIfGreaterThanZero(tableColumn.precision(), o -> entityColumn.precision = o);
      NumberUtils.doIfGreaterThanZero(tableColumn.scale(), o -> entityColumn.scale = o);

      entityColumn.nullable = entityColumn.nullable && tableColumn.nullable();
      entityColumn.unique = tableColumn.unique();

      if (entityColumn.columnDefinition == null) {
        entityColumn.typeName =
            entityColumn.supported
                ? dialect.getTypeName(
                    entityColumn.getType(field, entityColumn.type),
                    entityColumn.length,
                    entityColumn.precision,
                    entityColumn.scale)
                : null;
      }
    }
    entityColumn.columnDefinition = entityColumn.getColumnDefinition(dialect);
    // 默认主键定义
    if (entityColumn.isPrimaryKey()) {
      entityColumn.columnDefinition = getPrimaryKeyDefinition(dialect, entityColumn);
    }
    return entityColumn;
  }

  private Type getType(Field field, Type type) {
    if (this.type instanceof Class && InterEnum.class.isAssignableFrom((Class<?>) this.type)) {
      return ((ParameterizedType) field.getType().getGenericInterfaces()[0])
          .getActualTypeArguments()[0];
    }
    return this.type;
  }

  private String getColumnDefinition(final Dialect dialect) {
    if (columnDefinition != null) {
      return columnDefinition;
    }
    StringBuilder definition = new StringBuilder();
    definition.append(typeName);
    if (!nullable) {
      definition.append(" not null");
    }
    if (defaultValue != null) {
      definition.append(" default ").append(defaultValue);
    }
    if (!dialect.isSupportCommentOn() && StringUtils.nonEmpty(comment)) {
      definition.append(" comment ").append(comment);
    }
    return definition.toString();
  }

  @Nonnull
  private static Field getReferenceField(final @Nonnull Field field) {
    final ManyToOne manyToOne = field.getAnnotation(ManyToOne.class);
    final Class<?> fieldType = field.getType();
    if (manyToOne != null) {
      final String referenceField = manyToOne.targetField();
      if (!referenceField.equals("")) {
        return Objects.requireNonNull(
            BeanUtils.getDeclaredField(fieldType, referenceField),
            "Not exist field [" + referenceField + "] in " + fieldType);
      }
    } else {
      final OneToOne oneToOne = field.getAnnotation(OneToOne.class);
      final String referenceField = oneToOne.targetField();
      if (!referenceField.equals("")) {
        return Objects.requireNonNull(
            BeanUtils.getDeclaredField(fieldType, referenceField),
            "Not exist field [" + referenceField + "] in " + fieldType);
      }
    }
    // 关联主键
    return EntityHelper.getEntityInfo(fieldType).getPrimaryKeys().get(0).getField();
  }

  private static String getPrimaryKeyDefinition(
      final Dialect dialect, final EntityColumn entityColumn) {
    final Type type = entityColumn.type;
    // 只有主键类型是整型时,才可能自增
    if (type instanceof Class && !Number.class.isAssignableFrom((Class<?>) type)) {
      return entityColumn.columnDefinition;
    }
    final IdGenerator idGenerator = AppContextInject.getBean(IdGenerator.class);
    Object id = 1L;
    try {
      id = idGenerator.nextId(null);
    } catch (Exception ignore) {
      // 用户自定义的id生成器可能会用到obj参数,此时会抛异常,认为主键非自增
    }
    // 只有id为null时才拼接自增定义
    final IdentityColumnSupport identityColumnSupport = dialect.getIdentityColumnSupport();
    return id == null
        ? identityColumnSupport.containDataTypeInIdentityColumn()
            ? identityColumnSupport.getIdentityColumnString(type)
            : dialect
                .getTypeName(type)
                .concat(" ")
                .concat(identityColumnSupport.getIdentityColumnString(type))
        : entityColumn.columnDefinition;
  }

  @Override
  public boolean equals(final Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    final EntityColumn column = (EntityColumn) o;
    return Objects.equals(name, column.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }
}
