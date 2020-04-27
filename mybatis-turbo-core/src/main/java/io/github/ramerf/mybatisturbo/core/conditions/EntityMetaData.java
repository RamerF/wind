package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import lombok.Data;

/**
 * .
 *
 * @author Tang Xiaofeng
 * @since 2020/4/11
 */
@Data
public class EntityMetaData<T extends AbstractEntity> {
  protected String tableName;
  protected String tableAlia;
  protected Class<T> clazz;
}
