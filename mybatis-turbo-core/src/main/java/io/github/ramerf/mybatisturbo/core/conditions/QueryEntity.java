package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
public interface QueryEntity<T extends AbstractEntity> {
  String getString();

  QueryEntityMetaData<T> getQueryEntityMetaData();
}
