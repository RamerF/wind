package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.support.VarArgsFunction;

/**
 * sql aggregate function.<br>
 * <code>Count</code>,<code>Sum</code>,<code>Avg</code>,<code>Max</code>, <code>Min</code>
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
@SuppressWarnings({"unused"})
public enum SqlAggregateFunction implements SqlFunction {
  /** COUNT(%s) */
  COUNT(str -> " count(" + String.join("", str) + ") "),
  SUM(str -> " sum(" + String.join("", str) + ") "),
  MAX(str -> " max(" + String.join("", str) + ") "),
  MIN(str -> " min(" + String.join("", str) + ") "),
  AVG(str -> " avg(" + String.join("", str) + ") "),
  EXISTS(str -> " exists(" + String.join("", str) + ") "),
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
