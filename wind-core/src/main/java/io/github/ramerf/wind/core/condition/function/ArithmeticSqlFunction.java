package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.support.VarArgsFunction;

/**
 * 别看了,全都不支持.
 *
 * <pre>
 * <code>abs</code>,<code>ceil</code>,<code>floor</code>,<code>exp</code>,<code>ln</code>,
 * <code>mod</code>,<code>power</code>,<code>sqrt</code>
 * </pre>
 *
 * @author ramer
 * @since 2020/4/29
 */
public enum ArithmeticSqlFunction implements SqlFunction {
  /** 取绝对值. */
  ABS(str -> " abs(" + String.join("", str) + ") "),
  ;

  private final VarArgsFunction<String, String> exec;

  ArithmeticSqlFunction(final VarArgsFunction<String, String> exec) {
    this.exec = exec;
  }

  @Override
  public String string(final String... str) {
    return exec.apply(str);
  }
}
