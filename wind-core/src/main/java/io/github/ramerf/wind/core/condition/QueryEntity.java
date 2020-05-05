package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
public interface QueryEntity<T extends AbstractEntity> {
  String getString();

  QueryEntityMetaData<T> getQueryEntityMetaData();
}
