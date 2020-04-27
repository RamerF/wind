package io.github.ramerf.mybatisturbo.core.util;

/**
 * Stream 工具类.
 *
 * @author Tang Xiaofeng
 * @since 2020/2/12
 */
public class StreamUtils {
  /**
   * stream.map()操作时返回当前元素,并执行额外操作.
   *
   * @param o map参数
   * @param runnable 额外操作
   * @param <T> map参数类型
   * @return o
   */
  public static <T> T mapOpt(T o, Runnable runnable) {
    runnable.run();
    return o;
  }
}
