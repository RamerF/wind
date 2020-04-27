package io.github.ramerf.mybatisturbo.core.handler;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * BigDecimal->Double转换.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class BigDecimalTypeConverter implements TypeConverter<BigDecimal, Double> {
  @Override
  public Double convertToJdbc(BigDecimal val) {
    return Objects.nonNull(val) ? val.doubleValue() : null;
  }

  @Override
  public BigDecimal covertFromJdbc(final Double val, final Class<? extends BigDecimal> clazz) {
    return Objects.nonNull(val) ? BigDecimal.valueOf(val) : null;
  }
}
