package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.annotation.UpdateTimestamp;
import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.reflect.Field;
import java.util.Map;
import javax.annotation.Nonnull;
import lombok.Data;

/**
 * 实体信息.
 *
 * @author ramer
 * @since 2020/7/24
 */
@Data
public final class EntityInfo {

  /** 表名. */
  private String name;

  /** 逻辑删除. */
  LogicDeleteProp logicDeleteProp = new LogicDeleteProp();

  /** 更新时间字段,{@link UpdateTimestamp} */
  private Field updateTimeFiled;

  private Field createTimeField;

  /** 字段与列名映射 {field:column}. */
  private Map<String, String> fieldColumnMap;

  private EntityInfo() {}

  public static EntityInfo of(@Nonnull final WindConfiguration configuration) {
    EntityInfo entityInfo = new EntityInfo();
    final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
    logicDeleteProp.setEnable(configuration.getLogicDeleteProp().isEnable());
    logicDeleteProp.setColumn(configuration.getLogicDeleteProp().getColumn());
    logicDeleteProp.setDeleted(configuration.getLogicDeleteProp().isDeleted());
    logicDeleteProp.setNotDelete(configuration.getLogicDeleteProp().isNotDelete());
    return entityInfo;
  }

  public static EntityInfo of(@Nonnull final Class<?> clazz, Map<String, String> fieldColumnMap) {
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.setName(EntityUtils.getTableName(clazz));
    entityInfo.setFieldColumnMap(fieldColumnMap);
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null) {
      final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
      logicDeleteProp.setEnable(tableInfo.logicDelete().enable());
      logicDeleteProp.setColumn(tableInfo.logicDelete().column());
      logicDeleteProp.setDeleted(tableInfo.logicDelete().deleted());
      logicDeleteProp.setNotDelete(tableInfo.logicDelete().notDelete());
    }
    return entityInfo;
  }
}
