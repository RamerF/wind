package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.support.VarArgsFunction;

/**
 * sql character function.<br>
 * <code>lower</code>,<code>upper</code>,<code>trim</code>,<code>translate
 * </code>
 *
 * @author ramer
 * @since 2020/4/29
 */
public enum SqlCharacterFunction implements SqlFunction {
  /** 取小写字母. */
  LOWER(str -> " lower(" + String.join("", str) + ") "),
  UPPER(str -> " upper(" + String.join("", str) + ") "),
  TRIM(str -> " trim(" + String.join("", str) + ") "),
  TRANSLATE(str -> " translate(" + String.join("", str) + ") "),
  ;
  private final VarArgsFunction<String, String> exec;

  SqlCharacterFunction(final VarArgsFunction<String, String> exec) {
    this.exec = exec;
  }

  @Override
  public VarArgsFunction<String, String> init() {
    return this.exec;
  }
}
