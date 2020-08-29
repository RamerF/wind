package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.annotation.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.config.WindConfiguration.CommonField;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.util.*;
import javax.annotation.Nonnull;
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

  /** 表名. */
  private String name;

  /** 备注. */
  private String comment;

  /** 逻辑删除. */
  LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** 更新时间字段,{@link UpdateTimestamp} */
  private Field updateTimeField;

  private Field createTimeField;

  /** 是否映射到数据库. */
  private boolean mapToTable = true;

  /** 字段与列名映射 {field:column}. */
  private Map<String, String> fieldColumnMap;

  /** 列信息. */
  private List<EntityColumn> entityColumns;

  /** 主键. */
  private List<EntityColumn> primaryKeys;

  private Dialect dialect;

  /** 默认创建时间字段. */
  public static final Field DEFAULT_CREATE_TIME_FIELD;

  /** 默认更新时间字段. */
  public static final Field DEFAULT_UPDATE_TIME_FIELD;

  /** 默认逻辑删除字段. */
  public static final Field DEFAULT_LOGIC_DELETE_FIELD;

  static {
    try {
      DEFAULT_CREATE_TIME_FIELD = AbstractEntityPoJo.class.getDeclaredField("createTime");
      DEFAULT_UPDATE_TIME_FIELD = AbstractEntityPoJo.class.getDeclaredField("updateTime");
      DEFAULT_LOGIC_DELETE_FIELD = AbstractEntityPoJo.class.getDeclaredField("deleted");
    } catch (NoSuchFieldException e) {
      throw CommonException.of(e.getMessage(), e);
    }
  }

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
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.mapToTable = EntityHelper.isMapToTable(clazz);
    entityInfo.dialect = dialect;
    entityInfo.setClazz(clazz);
    entityInfo.setName(EntityUtils.getTableName(clazz));
    entityInfo.setComment(
        Optional.ofNullable(clazz.getAnnotation(TableInfo.class))
            .map(TableInfo::comment)
            .orElse(null));

    Map<String, String> fieldColumnMap = new HashMap<>(10);
    // 0:创建时间 1:更新时间
    final Field[] timeField = new Field[2];
    final List<Field> columnFields = EntityUtils.getAllColumnFields(clazz);
    columnFields.forEach(
        field -> {
          if (field.getAnnotation(CreateTimestamp.class) != null) {
            timeField[0] = field;
          }
          if (field.getAnnotation(UpdateTimestamp.class) != null) {
            timeField[1] = field;
          }
          fieldColumnMap.put(field.getName(), EntityUtils.fieldToColumn(field));
        });
    getCreateUpdateTimeFields(timeField, configuration);
    entityInfo.setCreateTimeField(timeField[0]);
    entityInfo.setUpdateTimeField(timeField[1]);
    entityInfo.setFieldColumnMap(fieldColumnMap);
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null) {
      entityInfo.setLogicDeleteProp(LogicDeleteProp.of(tableInfo, configuration));
    } else {
      entityInfo.setLogicDeleteProp(LogicDeleteProp.of(configuration));
    }
    if (!entityInfo.getLogicDeleteProp().isEnable() && entityInfo.isMapToTable()) {
      log.warn("表[{}]将使用物理删除!", entityInfo.name);
    }
    List<EntityColumn> primaryKeys = new ArrayList<>();
    List<EntityColumn> entityColumns = new ArrayList<>();
    for (Field field : columnFields) {
      final EntityColumn entityColumn = EntityColumn.of(field, dialect);
      if (entityColumn.isPrimaryKey()) {
        primaryKeys.add(entityColumn);
      }
      entityColumns.add(entityColumn);
    }
    entityInfo.setPrimaryKeys(primaryKeys);
    entityInfo.setEntityColumns(entityColumns);
    return entityInfo;
  }

  /** 获取创建/更新时间字段.如果字段为空且默认字段未被禁用,将会使用默认字段. */
  private static void getCreateUpdateTimeFields(
      final Field[] timeField, final WindConfiguration configuration) {
    final List<CommonField> disableFields = configuration.getDisableFields();
    if (timeField[0] == null
        && disableFields.stream()
            .noneMatch(disableField -> disableField.getField().equals(DEFAULT_CREATE_TIME_FIELD))) {
      timeField[0] = DEFAULT_CREATE_TIME_FIELD;
    }
    if (timeField[1] == null
        && disableFields.stream()
            .noneMatch(disableField -> disableField.getField().equals(DEFAULT_UPDATE_TIME_FIELD))) {
      timeField[1] = DEFAULT_UPDATE_TIME_FIELD;
    }
  }
}
