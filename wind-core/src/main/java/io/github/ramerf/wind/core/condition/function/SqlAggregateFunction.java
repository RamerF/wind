package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.support.VarArgsFunction;

/**
 * sql aggregate function.<br>
 * <code>Count</code>,<code>Sum</code>,<code>Avg</code>,<code>Max</code>, <code>Min</code>
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
@SuppressWarnings({"RedundantCast", "unused"})
public enum SqlAggregateFunction implements SqlFunction {
  /** COUNT(%s) */
  COUNT(str -> " COUNT(" + String.join("", str) + ") "),
  SUM(str -> " SUM(" + String.join("", str) + ") "),
  MAX(str -> " MAX(" + String.join("", str) + ") "),
  MIN(str -> " MIN(" + String.join("", str) + ") "),
  AVG(str -> " AVG(" + String.join("", str) + ") "),
  EXISTS(str -> " EXISTS(" + String.join("", str) + ") "),
  ;
  private final VarArgsFunction<String, String> exec;

  SqlAggregateFunction(final VarArgsFunction<String, String> exec) {
    this.exec = exec;
  }

  @Override
  public VarArgsFunction<String, String> init() {
    return this.exec;
  }
}
