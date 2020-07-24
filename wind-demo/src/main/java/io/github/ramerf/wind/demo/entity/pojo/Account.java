package io.github.ramerf.wind.demo.entity.pojo;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Tang Xiaofeng
 * @since 2020/07/24
 */
@TableInfo(
    value = "account",
    logicDeleteColumn = "has_deleted",
    logicDeleted = false,
    logicNotDelete = true)
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Account extends AbstractEntityPoJo {

  private String name;

  private String tel;

  /** 使用自定义逻辑删除字段. */
  private Boolean hasDeleted;
}
