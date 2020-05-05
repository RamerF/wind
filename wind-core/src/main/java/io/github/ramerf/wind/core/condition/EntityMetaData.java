package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;
import lombok.Data;

/**
 * .
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /4/11
 */
@Data
public class EntityMetaData<T extends AbstractEntity> {
  /** 表名. */
  protected String tableName;

  /** 表别名. */
  protected String tableAlia;

  /** 表对应的实体类. */
  protected Class<T> clazz;
}
