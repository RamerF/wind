package io.github.ramerf.wind.core.condition;

import lombok.Data;

/**
 * .
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2020 /4/11
 */
@Data
public class EntityMetaData<T> {
  /** 表名. */
  protected String tableName;

  /** 表别名. */
  protected String tableAlia;

  /** 表对应的实体类. */
  protected Class<T> clazz;
}
