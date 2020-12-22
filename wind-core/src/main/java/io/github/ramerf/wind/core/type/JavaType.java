package io.github.ramerf.wind.core.type;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.util.List;
import java.util.Set;

import static io.github.ramerf.wind.core.util.TypeUtils.getType;

/**
 * Java类型.用于与sql类型对应.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.21
 */
public interface JavaType {
  Type LIST_SHORT = getType(List.class, Short.class);
  Type LIST_INTEGER = getType(List.class, Integer.class);
  Type LIST_LONG = getType(List.class, Long.class);
  Type LIST_FLOAT = getType(List.class, Float.class);
  Type LIST_DOUBLE = getType(List.class, Double.class);
  Type LIST_BIGDECIMAL = getType(List.class, BigDecimal.class);
  Type LIST_STRING = getType(List.class, String.class);

  Type SET_SHORT = getType(Set.class, Short.class);
  Type SET_INTEGER = getType(Set.class, Integer.class);
  Type SET_LONG = getType(Set.class, Long.class);
  Type SET_FLOAT = getType(Set.class, Float.class);
  Type SET_DOUBLE = getType(Set.class, Double.class);
  Type SET_BIGDECIMAL = getType(Set.class, BigDecimal.class);
  Type SET_STRING = getType(Set.class, String.class);

  /**
   * 可映射的数据库类型.
   *
   * @return the string [ ]
   */
  String[] getSqlTypes();
}
