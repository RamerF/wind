package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.support.VarArgsFunction;

/**
 * 别看了,全都不支持.
 *
 * <pre>
 * <code>abs</code>,<code>ceil</code>,<code>floor</code>,<code>exp</code>,<code>ln</code>,
 * <code>mod</code>,<code>power</code>,<code>sqrt</code>
 * </pre>
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
@SuppressWarnings("RedundantCast")
public enum SqlArithmeticFunction implements SqlFunction {
  ABS(
      str -> {
        return String.format(" SUM(%s) ", (Object[]) str);
      }),
  ;
  //  private final String function;
  private final VarArgsFunction<String, String> exec;

  SqlArithmeticFunction(final VarArgsFunction<String, String> exec) {
    this.exec = exec;
  }

  @Override
  public VarArgsFunction<String, String> init() {
    return this.exec;
  }
}
