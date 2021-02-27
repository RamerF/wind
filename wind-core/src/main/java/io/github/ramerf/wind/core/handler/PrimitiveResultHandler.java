package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import io.github.ramerf.wind.core.handler.PrimitiveResultHandler.EmptyPoJo;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.Map.Entry;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 基本类型结果转换,注意该类使用valueOf转换值,如果没有该方法将会报错.
 *
 * @param <E> the type parameter
 * @author ramer
 * @since 2019 /12/27
 */
@Slf4j
public class PrimitiveResultHandler<E>
    extends AbstractResultHandler<EmptyPoJo, Map<String, Object>, E> {

  /**
   * Instantiates a new Primitive result handler.
   *
   * @param clazz the clazz
   */
  public PrimitiveResultHandler(@Nonnull final Class<E> clazz) {
    super(clazz, null, false);
  }

  @Override
  @SuppressWarnings({"unchecked", "rawtypes"})
  public E handle(Map<String, Object> map) {
    if (CollectionUtils.isEmpty(map)) {
      return null;
    }
    final Object value = map.entrySet().stream().findFirst().map(Entry::getValue).orElse(null);
    if (value == null) {
      return nullToZero();
    }
    final Class<?> valueClass = value.getClass();
    if (valueClass.equals(clazz)) {
      return (E) value;
    }
    if (value instanceof String) {
      return (E) String.valueOf(value);
    }
    if (value instanceof Long) {
      return (E) Long.valueOf(value.toString());
    }
    if (InterEnum.class.isAssignableFrom(clazz)) {
      final Class<? extends InterEnum> cls = (Class<? extends InterEnum>) clazz;
      return (E) InterEnum.ofNullable(value, cls);
    }
    if (valueClass.isArray()) {
      return (E) value;
    }
    // 使用构造器
    try {
      return clazz.getConstructor(valueClass).newInstance(value);
    } catch (Exception e) {
      log.warn("handle:[msg:{},class:{}]", e.getMessage(), e.getClass());
      // 使用valueOf
      try {
        return (E) clazz.getMethod("valueOf", valueClass).invoke(null, value);
      } catch (Exception ex) {
        log.warn("handle:[msg:{},class:{}]", ex.getMessage(), ex.getClass());
      }
    }
    return (E) value;
  }

  /** null转换为基本类型零值. */
  @SuppressWarnings("unchecked")
  private E nullToZero() {
    if (Long.class.isAssignableFrom(clazz)) {
      return (E) Long.valueOf(0);
    }
    if (Integer.class.isAssignableFrom(clazz)) {
      return (E) Integer.valueOf(0);
    }
    if (BigInteger.class.isAssignableFrom(clazz)) {
      return (E) BigInteger.valueOf(0);
    }
    if (BigDecimal.class.isAssignableFrom(clazz)) {
      return (E) BigDecimal.valueOf(0);
    }
    if (Double.class.isAssignableFrom(clazz)) {
      return (E) Double.valueOf(0);
    }
    return null;
  }

  public static class EmptyPoJo {}
}
