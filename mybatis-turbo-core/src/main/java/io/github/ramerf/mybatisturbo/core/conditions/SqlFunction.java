package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.support.VarArgsFunction;

/**
 * sql function.
 *
 * @author Tang Xiaofeng
 * @since 2020/4/29
 */
public interface SqlFunction {

  VarArgsFunction<String, String> init();

  default String string(final String... str) {
    return init().apply(str);
  }
}
