package io.github.ramerf.wind.core.serializer;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.entity.response.Rs;

/**
 * 定义枚举的序列化,自定义序列化实现该类即可.
 *
 * @author ramer
 */
@FunctionalInterface
public interface InterEnumSerializer {

  /**
   * 序列化json,默认值为{@link #defaultSerializer(InterEnum)}
   *
   * @param interEnum the {@link InterEnum}
   * @return json
   */
  Object serializer(InterEnum interEnum);

  /**
   * 序列化json:
   *
   * <pre>
   *   {
   *    value: value,
   *    desc: desc
   *   }
   * </pre>
   *
   * @param interEnum the {@link InterEnum}
   * @return json
   */
  default Object defaultSerializer(InterEnum interEnum) {
    return Rs.json().put("value", interEnum.value()).put("desc", interEnum.desc());
  }
}
