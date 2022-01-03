package io.github.ramerf.wind.core.function;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

/** Setter函数表达式缓存 */
@Data
@Slf4j
public class CachedSetterFunctions {
  private static final Map<Field, WeakReference<SetterFunction<?, ?>>> CONSUMER_MAP =
      new ConcurrentHashMap<>(new WeakHashMap<>());

  /** 使用缓存方法调用,如果没有缓存,返回false,表示未执行任何操作 */
  @SuppressWarnings({"rawtypes", "unchecked"})
  public static boolean invoke(final Field field, final Object t, final Object value) {
    WeakReference<SetterFunction<?, ?>> reference = CONSUMER_MAP.get(field);
    SetterFunction consumer;
    if (reference != null && (consumer = reference.get()) != null) {
      if (log.isTraceEnabled()) {
        log.debug("invoke:hit cache[{}]", field.getName());
      }
      consumer.accept(t, value);
      return true;
    }
    return false;
  }

  public static void put(final SetterFunction<?, ?> setterFunction) {
    Field field = setterFunction.getField();
    Class<?> clazz = field.getDeclaringClass();
    Optional.ofNullable(CONSUMER_MAP.get(field))
        .map(Reference::get)
        .orElseGet(
            () -> {
              CONSUMER_MAP.put(field, new WeakReference<>(setterFunction));
              return null;
            });
  }
}
