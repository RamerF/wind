package io.github.ramerf.wind.core.support;

/**
 * id生成策略.
 *
 * @author ramer
 * @since 2020 /5/20
 */
@FunctionalInterface
public interface IdGenerator {
  /**
   * 生成id.
   *
   * @param obj 当前对象
   * @return the long
   */
  Object nextId(final Object obj);
}
