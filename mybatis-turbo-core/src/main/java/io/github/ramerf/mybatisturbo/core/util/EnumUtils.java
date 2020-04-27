package io.github.ramerf.mybatisturbo.core.util;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;

/**
 * 枚举工具类.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/11
 */
@Slf4j
public class EnumUtils {
  /**
   * Desc string.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param clazz the clazz
   * @param value the value
   * @return the string
   */
  public static <T extends Enum<?>, R> String desc(Class<T> clazz, final R value) {
    return map(clazz).get(value);
  }

  /**
   * 获取枚举实例, 返回null 值无效.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param clazz the clazz
   * @param value the value
   * @return t t
   */
  public static <T, R> T of(Class<T> clazz, final R value) {
    return Stream.of(clazz.getEnumConstants())
        .filter(
            o -> {
              try {
                return Objects.equals(clazz.getDeclaredMethod("value").invoke(o), value);
              } catch (Exception e) {
                e.printStackTrace();
              }
              return false;
            })
        .findFirst()
        .orElse(null);
  }

  /**
   * Map of value:desc.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param clazz the clazz
   * @return the map
   */
  @SuppressWarnings("unchecked")
  public static <T extends Enum<?>, R> Map<R, String> map(Class<T> clazz) {
    return Stream.of(clazz.getEnumConstants())
        .collect(
            Collectors.toMap(
                o -> {
                  try {
                    return (R) clazz.getDeclaredMethod("value").invoke(o);
                  } catch (Exception ignored) {
                    return null;
                  }
                },
                o -> {
                  try {
                    return (String) clazz.getDeclaredMethod("desc").invoke(o);
                  } catch (Exception ignored) {
                    return "";
                  }
                }));
  }

  /**
   * Values list.
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param clazz the clazz
   * @return the list
   */
  @SuppressWarnings("unchecked")
  public static <T extends Enum<?>, R> List<R> values(Class<T> clazz) {
    return Stream.of(clazz.getEnumConstants())
        .map(
            o -> {
              try {
                return (R) clazz.getDeclaredMethod("value").invoke(o);
              } catch (Exception ignored) {
                return null;
              }
            })
        .collect(Collectors.toList());
  }

  /**
   * 校验给定的值是否是枚举类的有效值.<br>
   * 注意: 只能校验枚举类value属性
   *
   * @param <T> the type parameter
   * @param <R> the type parameter
   * @param clazz 枚举类
   * @param v 校验值
   * @return true,有效值
   */
  @SuppressWarnings("unchecked")
  public static <T extends Enum<?>, R> boolean valid(Class<T> clazz, final R v) {
    return Stream.of(clazz.getEnumConstants())
        .map(
            o -> {
              try {
                return (R) clazz.getDeclaredMethod("value").invoke(o);
              } catch (Exception ignored) {
                log.warn("valid:[枚举类中必须定义value方法]");
                return null;
              }
            })
        .collect(Collectors.toList()).stream()
        .anyMatch(o -> Objects.equals(o, v));
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    log.info("main:map[{}]", EnumUtils.map(Type.class));
    log.info("main:of[{}]", EnumUtils.of(Type.class, 1));
    log.info("main:values[{}]", EnumUtils.values(Type.class));
    log.info("main:valid[{}]", EnumUtils.valid(Type.class, 2));
  }

  public enum Type {
    /** 商品类别 */
    PHONE(0, "手机"),
    SPORT(1, "运动");

    private int value;
    private String desc;

    Type(int value, String desc) {
      this.value = value;
      this.desc = desc;
    }

    public int value() {
      return this.value;
    }

    public String desc() {
      return this.desc;
    }
  }
}
