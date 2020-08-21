package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
import javax.persistence.Id;
import lombok.Data;

import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.CREATE_TIME_FIELD_NAME;
import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.UPDATE_TIME_FIELD_NAME;

/**
 * 实体信息.
 *
 * @author ramer
 * @since 2020/7/24
 */
@Data
public final class EntityInfo {
  private Class<?> clazz;

  /** 表名. */
  private String name;

  /** 逻辑删除. */
  LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** 更新时间字段,{@link UpdateTimestamp} */
  private Field updateTimeFiled;

  private Field createTimeField;

  /** 字段与列名映射 {field:column}. */
  private Map<String, String> fieldColumnMap;

  /** 列信息. */
  private List<EntityColumn> entityColumns;

  /** 主键. */
  private List<EntityColumn> primaryKeys;

  private Dialect dialect;

  public static EntityInfo of(@Nonnull final WindConfiguration configuration) {
    return of(configuration, null);
  }

  public static EntityInfo of(@Nonnull final WindConfiguration configuration, Dialect dialect) {
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.dialect = dialect;
    entityInfo.setLogicDeleteProp(LogicDeleteProp.of(configuration));
    return entityInfo;
  }

  public static EntityInfo of(
      @Nonnull final Class<?> clazz, final WindConfiguration configuration, Dialect dialect) {
    Map<String, String> fieldColumnMap = new HashMap<>(10);
    // 0:创建时间 1:更新时间
    final Field[] timeField = new Field[2];
    // 默认时间字段
    final Field[] defaultTimeField = new Field[2];
    final List<Field> columnFields = EntityUtils.getAllColumnFields(clazz);
    columnFields.forEach(
        field -> {
          if (field.getAnnotation(CreateTimestamp.class) != null) {
            timeField[0] = field;
          }
          if (field.getAnnotation(UpdateTimestamp.class) != null) {
            timeField[1] = field;
          }
          if (field.getName().equals(CREATE_TIME_FIELD_NAME)) {
            defaultTimeField[0] = field;
          }
          if (field.getName().equals(UPDATE_TIME_FIELD_NAME)) {
            defaultTimeField[1] = field;
          }
          fieldColumnMap.put(field.getName(), EntityUtils.fieldToColumn(field));
        });

    EntityInfo entityInfo = new EntityInfo();
    entityInfo.dialect = dialect;
    entityInfo.setCreateTimeField(timeField[0] == null ? defaultTimeField[0] : timeField[0]);
    entityInfo.setUpdateTimeFiled(timeField[1] == null ? defaultTimeField[1] : timeField[1]);
    entityInfo.setClazz(clazz);
    entityInfo.setName(EntityUtils.getTableName(clazz));
    entityInfo.setFieldColumnMap(fieldColumnMap);
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null) {
      entityInfo.setLogicDeleteProp(LogicDeleteProp.of(tableInfo));
    } else {
      entityInfo.setLogicDeleteProp(LogicDeleteProp.of(configuration));
    }

    List<EntityColumn> primaryKeys = new ArrayList<>();
    List<EntityColumn> entityColumns = new ArrayList<>();
    for (Field field : columnFields) {
      final EntityColumn entityColumn = EntityColumn.of(field, dialect);
      if (field.getAnnotation(Id.class) != null) {
        primaryKeys.add(entityColumn);
      }
      entityColumns.add(entityColumn);
    }
    entityInfo.setPrimaryKeys(primaryKeys);
    entityInfo.setEntityColumns(entityColumns);
    return entityInfo;
  }
}
