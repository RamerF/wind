package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.LogicDelete;
import io.github.ramerf.wind.core.annotation.TableInfo;
import java.io.Serializable;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

/**
 * 逻辑删除配置.
 *
 * @author ramer
 * @since 2020/7/26
 */
@Slf4j
@Setter
@Getter
public class LogicDeleteProp {
  /** 是否开启逻辑删除. */
  private boolean enable = false;

  /** 逻辑删除字段. */
  private String fieldName;

  /** 逻辑未删除值. */
  private boolean notDelete = false;

  /** 逻辑已删除值. */
  private boolean deleted = true;

  public static LogicDeleteProp of(@Nonnull final WindConfiguration configuration) {
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(configuration.getLogicDeleteProp().isEnable());
    logicDeleteProp.setFieldName(configuration.getLogicDeleteProp().getFieldName());
    logicDeleteProp.setDeleted(configuration.getLogicDeleteProp().isDeleted());
    logicDeleteProp.setNotDelete(configuration.getLogicDeleteProp().isNotDelete());
    return logicDeleteProp;
  }

  public static LogicDeleteProp of(
      final TableInfo tableInfo, @Nonnull final WindConfiguration configuration) {
    LogicDelete logicDelete;
    // 如果fieldName为默认值XX_FIELD_NAME_XX,说明未指定该属性,使用全局配置
    if (tableInfo == null || (logicDelete = tableInfo.logicDelete()).fieldName().equals("")) {
      return of(configuration);
    }
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(logicDelete.enable());
    logicDeleteProp.setFieldName(logicDelete.fieldName());
    logicDeleteProp.setDeleted(logicDelete.deleted());
    logicDeleteProp.setNotDelete(logicDelete.notDelete());
    return logicDeleteProp;
  }

  public <ID extends Serializable> ID getIdClass(final Class<?> clazz) {
    throw new RuntimeException("Not implemented");
  }
}
