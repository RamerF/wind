package io.github.ramerf.wind.core.type;

/**
 * Java类型.用于与sql类型对应.
 *
 * @author Tang Xiaofeng
 * @since 2020.08.21
 */
public class IntegerType implements JavaType {
  @Override
  public String[] getSqlTypes() {
    return new String[] {};
  }
}
