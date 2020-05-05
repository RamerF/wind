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
  COUNT(str -> String.format(" COUNT(%s) ", (Object[]) str)),
  SUM(str -> String.format(" SUM(%s) ", (Object[]) str)),
  MAX(str -> String.format(" MAX(%s) ", (Object[]) str)),
  MIN(str -> String.format(" MIN(%s) ", (Object[]) str)),
  AVG(str -> String.format(" AVG(%s) ", (Object[]) str)),
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
