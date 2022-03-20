package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.mapping.EntityMapping.MappingInfo;
import io.github.ramerf.wind.core.support.IdGenerator.VoidIdGenerator;
import io.github.ramerf.wind.core.util.BeanUtils;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
import javax.persistence.Id;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/**
 * 实体信息.
 *
 * @author ramer
 * @since 2020/7/24
 */
@Data
@Slf4j
public final class EntityInfo {
  private Class<?> clazz;
  /** 主键生成类. */
  private IdGenerator idGenerator;
  /** 表名. */
  private String name;

  /** 主键列信息. */
  private EntityColumn idColumn;

  /** 备注. */
  private String comment;

  /** 逻辑删除. */
  LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** 逻辑删除属性列,如果该表不支持逻辑删除,返回null. */
  private EntityColumn logicDeletePropColumn;

  /** 更新时间字段,{@link UpdateTimestamp} */
  private Field updateTimeField;

  /** 创建时间字段,{@link CreateTimestamp} */
  private Field createTimeField;

  /** 是否映射到数据库. */
  private boolean mapToTable = true;

  /** 字段与列映射 {Field:EntityColumn}. */
  private Map<Field, EntityColumn> fieldColumnMap;

  /** 列名与字段映射 {column:field}. */
  private Map<String, Field> columnFieldMap;

  /** 列信息. */
  private List<EntityColumn> entityColumns;

  /** 索引信息. */
  private List<EntityIndex> entityIndexes;

  /** 主键. */
  private List<EntityColumn> primaryKeys;

  /** 关联对象. */
  private List<MappingInfo> mappingInfos = new ArrayList<>();

  private Dialect dialect;

  public static EntityInfo of(@Nonnull final Configuration configuration) {
    return of(configuration, null);
  }

  public static EntityInfo of(@Nonnull final Configuration configuration, Dialect dialect) {
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.dialect = dialect;
    entityInfo.setLogicDeleteProp(LogicDeleteProp.of(configuration));
    return entityInfo;
  }

  public static EntityInfo of(
      @Nonnull final Class<?> clazz, final Configuration configuration, Dialect dialect) {
    EntityInfo entityInfo = new EntityInfo();
    final TableInfo annotation = clazz.getAnnotation(TableInfo.class);
    entityInfo.setLogicDeleteProp(LogicDeleteProp.of(annotation, configuration));

    entityInfo.mapToTable = EntityHelper.isMapToTable(clazz);
    entityInfo.dialect = dialect;
    entityInfo.setClazz(clazz);
    entityInfo.setName(EntityUtils.getTableName(clazz));
    if (annotation == null || annotation.idGenerator().equals(VoidIdGenerator.class)) {
      entityInfo.setIdGenerator(configuration.getIdGenerator());
    } else {
      entityInfo.setIdGenerator(BeanUtils.initial(annotation.idGenerator()));
    }
    if (annotation != null) {
      entityInfo.setComment(annotation.comment());
    }
    final List<Field> columnFields = EntityUtils.getAllColumnFields(clazz, null);
    Map<Field, EntityColumn> fieldColumnMap = new HashMap<>(20);
    Map<String, Field> columnFieldMap = new HashMap<>(20);
    // 0:创建时间 1:更新时间
    final Field[] timeField = new Field[2];

    List<EntityColumn> primaryKeys = new ArrayList<>();
    Set<EntityColumn> entityColumns = new HashSet<>();
    final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
    final String logicDeletePropName = logicDeleteProp.getFieldName();
    for (Field field : columnFields) {
      // 创建/更新时间
      if (field.getAnnotation(CreateTimestamp.class) != null) {
        timeField[0] = field;
      }
      if (field.getAnnotation(UpdateTimestamp.class) != null) {
        timeField[1] = field;
      }
      // 列信息
      final EntityColumn entityColumn =
          EntityColumn.of(field, dialect, entityInfo.getIdGenerator());
      if (entityColumn == null) {
        continue;
      }
      fieldColumnMap.put(field, entityColumn);
      columnFieldMap.put(entityColumn.getName(), field);
      if (entityColumn.isPrimaryKey()) {
        primaryKeys.add(entityColumn);
      }
      if (field.getAnnotation(Id.class) != null) {
        entityInfo.setIdColumn(entityColumn);
      }
      // 逻辑删除字段
      if (field.getName().equals(logicDeletePropName)) {
        entityInfo.setLogicDeletePropColumn(entityColumn);
      }
      entityColumns.add(entityColumn);
    }
    if (entityInfo.getIdColumn() == null) {
      throw new IllegalStateException(
          "Not found Identity for " + entityInfo.getName() + ".Define the @Id field.");
    }
    if (logicDeleteProp.isEnable() && entityInfo.getLogicDeletePropColumn() == null) {
      throw new IllegalStateException(
          String.format(
              "Not found logic delete prop [%s] for %s",
              logicDeleteProp.getFieldName(), entityInfo.getName()));
    }
    // 索引信息
    List<EntityIndex> entityIndexes =
        EntityIndex.getEntityIndexes(clazz, entityInfo.getName(), entityColumns, dialect);
    entityInfo.setEntityIndexes(entityIndexes);

    entityInfo.setPrimaryKeys(primaryKeys);
    entityInfo.setEntityColumns(new ArrayList<>(entityColumns));

    entityInfo.setCreateTimeField(timeField[0]);
    entityInfo.setUpdateTimeField(timeField[1]);
    entityInfo.setFieldColumnMap(fieldColumnMap);
    entityInfo.setColumnFieldMap(columnFieldMap);
    return entityInfo;
  }
}
