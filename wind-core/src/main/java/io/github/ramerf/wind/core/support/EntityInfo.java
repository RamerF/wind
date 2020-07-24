package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.util.EntityUtils;
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

  /** 是否开启逻辑删除. */
  private Boolean enableLogicDelete;

  /** 逻辑删除列. */
  private String logicDeleteColumn;

  /** 逻辑未删除值. */
  private boolean logicNotDelete;

  /** 逻辑已删除值. */
  private boolean logicDeleted;

  /** 字段与列名映射 {field:column}. */
  private Map<String, String> fieldColumnMap;

  private EntityInfo() {}

  public static EntityInfo of(@Nonnull final WindConfiguration configuration) {
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.logicDeleteColumn = configuration.getLogicDeleteColumn();
    entityInfo.logicDeleted = configuration.isLogicDeleted();
    entityInfo.logicNotDelete = configuration.isLogicNotDelete();
    entityInfo.enableLogicDelete = configuration.isEnableLogicDelete();
    return entityInfo;
  }

  public static EntityInfo of(@Nonnull final Class<?> clazz, Map<String, String> fieldColumnMap) {
    EntityInfo entityInfo = new EntityInfo();
    entityInfo.setName(EntityUtils.getTableName(clazz));
    entityInfo.setFieldColumnMap(fieldColumnMap);
    final TableInfo tableInfo = clazz.getAnnotation(TableInfo.class);
    if (tableInfo != null) {
      entityInfo.setEnableLogicDelete(tableInfo.enableLogicDelete());
      entityInfo.setLogicDeleteColumn(tableInfo.logicDeleteColumn());
      entityInfo.setLogicDeleted(tableInfo.logicDeleted());
      entityInfo.setLogicNotDelete(tableInfo.logicNotDelete());
    }
    return entityInfo;
  }
}
