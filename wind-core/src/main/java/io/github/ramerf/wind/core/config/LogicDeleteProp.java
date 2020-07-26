package io.github.ramerf.wind.core.config;

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
}
