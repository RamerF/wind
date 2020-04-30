package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.support.VarArgsFunction;

/**
 * sql character function.<br>
 * <code>lower</code>,<code>upper</code>,<code>trim</code>,<code>translate
 * </code>
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
@SuppressWarnings("RedundantCast")
public enum SqlCharacterFunction implements SqlFunction {
  LOWER(
      str -> {
        return String.format(" LOWER(%s) ", (Object[]) str);
      }),
  UPPER(
      str -> {
        return String.format(" UPPER(%s) ", (Object[]) str);
      }),
  TRIM(
      str -> {
        return String.format(" TRIM(%s) ", (Object[]) str);
      }),
  TRANSLATE(
      str -> {
        return String.format(" TRANSLATE(%s) ", (Object[]) str);
      }),
  ;
  //  private final String function;
  private final VarArgsFunction<String, String> exec;

  SqlCharacterFunction(final VarArgsFunction<String, String> exec) {
    this.exec = exec;
  }

  @Override
  public VarArgsFunction<String, String> init() {
    return this.exec;
  }
}
