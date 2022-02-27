package io.github.ramerf.wind.core.serializer;

import io.github.ramerf.wind.core.domain.InterEnum;

/**
 * 定义枚举的序列化,自定义序列化实现该类即可.
 *
 * <pre>
 * 序列化json:
 *
 * {
 *  value: value,
 *  desc: desc
 * }
 * </pre>
 *
 * @author ramer
 */
@FunctionalInterface
public interface InterEnumSerializer {

  /**
   * 序列化json,默认值为:{@link InterEnum#value()}
   *
   * @param interEnum the {@link InterEnum}
   * @return json
   */
  Object serializer(InterEnum<?> interEnum);
}
