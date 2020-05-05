package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.support.VarArgsFunction;

/**
 * sql function.
 *
 * @author Tang Xiaofeng
 * @since 2020 /4/29
 */
public interface SqlFunction {

  /**
   * Always <br>
   * <code>return this.exec;</code>
   *
   * @return the var args function
   */
  VarArgsFunction<String, String> init();

  /**
   * String string.
   *
   * @param str the str
   * @return the string
   */
  default String string(final String... str) {
    return init().apply(str);
  }
}
