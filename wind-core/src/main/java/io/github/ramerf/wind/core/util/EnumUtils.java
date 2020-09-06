package io.github.ramerf.wind.core.util;

import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.*;
import lombok.extern.slf4j.Slf4j;

import static java.util.stream.Collectors.toMap;

/**
 * 枚举工具类.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/11
 */
@Slf4j
public class EnumUtils {
  @SuppressWarnings("rawtypes")
  private static final Map<Class<? extends InterEnum<?>>, WeakReference<Map>> MAP = new HashMap<>();

  /**
   * 通过值获取枚举实例.<b>注意:匹配时值会被转换成字符串</b>
   *
   * @param <V> the type parameter
   * @param <E> the type parameter
   * @param value the value
   * @param clazz the clazz
   * @return the e
   */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public static <V, E extends InterEnum<V>> E of(V value, Class<E> clazz) {
    if (value == null) {
      return null;
    }
    return (E)
        Optional.ofNullable(MAP.get(clazz))
            .map(Reference::get)
            .map(o -> o.get(value.toString()))
            .orElseGet(
                () -> {
                  final Map clazzMap =
                      Arrays.stream(clazz.getEnumConstants())
                          .collect(toMap(o -> o.value().toString(), o -> o));
                  MAP.put(clazz, new WeakReference<>(clazzMap));
                  return clazzMap.get(value.toString());
                });
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    log.info("main:valid[{}]", EnumUtils.of(2, Type.class));
  }

  /** The enum Type. */
  public enum Type implements InterEnum<Integer> {
    /** 类别 */
    PHONE(0, "手机"),
    /** Sport type. */
    SPORT(1, "运动");

    private final int value;
    private final String desc;

    Type(int value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    @Override
    public Integer value() {
      return this.value;
    }

    @Override
    public String desc() {
      return this.desc;
    }
  }
}
