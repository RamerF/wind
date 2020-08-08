package io.github.ramerf.wind.core.config;

import io.github.ramerf.wind.core.annotation.TableInfo;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.Setter;

import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.LOGIC_DELETE_COLUMN_NAME;

/**
 * 逻辑删除配置.
 *
 * @author Tang Xiaofeng
 * @since 2020/7/26
 */
@Setter
@Getter
public class LogicDeleteProp {
  /** 是否开启逻辑删除. */
  private boolean enable = true;

  /** 逻辑删除字段. */
  private String column = LOGIC_DELETE_COLUMN_NAME;

  /** 逻辑未删除值. */
  private boolean notDelete = false;

  /** 逻辑已删除值. */
  private boolean deleted = true;

  public static LogicDeleteProp of(@Nonnull final WindConfiguration configuration) {
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(configuration.getLogicDeleteProp().isEnable());
    logicDeleteProp.setColumn(configuration.getLogicDeleteProp().getColumn());
    logicDeleteProp.setDeleted(configuration.getLogicDeleteProp().isDeleted());
    logicDeleteProp.setNotDelete(configuration.getLogicDeleteProp().isNotDelete());
    return logicDeleteProp;
  }

  public static LogicDeleteProp of(@Nonnull final TableInfo tableInfo) {
    LogicDeleteProp logicDeleteProp = new LogicDeleteProp();
    logicDeleteProp.setEnable(tableInfo.logicDelete().enable());
    logicDeleteProp.setColumn(tableInfo.logicDelete().column());
    logicDeleteProp.setDeleted(tableInfo.logicDelete().deleted());
    logicDeleteProp.setNotDelete(tableInfo.logicDelete().notDelete());
    return logicDeleteProp;
  }
}
