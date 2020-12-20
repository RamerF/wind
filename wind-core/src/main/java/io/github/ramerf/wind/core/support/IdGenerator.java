package io.github.ramerf.wind.core.support;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;

/**
 * id生成策略.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/20
 */
@FunctionalInterface
public interface IdGenerator {
  /**
   * 生成id.
   *
   * @param obj 当前对象
   * @return the long
   * @see AbstractEntityPoJo
   */
  Object nextId(final Object obj);
}
