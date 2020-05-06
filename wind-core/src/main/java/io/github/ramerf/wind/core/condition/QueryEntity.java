package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;

/**
 * The interface Query entity.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /12/28
 */
public interface QueryEntity<T extends AbstractEntity> {
  /**
   * Gets string.
   *
   * @return the string
   */
  String getString();

  /**
   * Gets query entity meta data.
   *
   * @return the query entity meta data
   */
  QueryEntityMetaData<T> getQueryEntityMetaData();
}
