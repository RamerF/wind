package io.github.ramerf.wind.core.converter;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * BigDecimal-&gt;Double转换.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class BigDecimalTypeConverter implements TypeConverter<BigDecimal, Double> {
  @Override
  public Double convertToJdbc(
      BigDecimal val, final Field field, @Nonnull final PreparedStatement ps) {
    return Objects.nonNull(val) ? val.doubleValue() : null;
  }

  @Override
  public BigDecimal covertFromJdbc(final Double val, final Class<? extends BigDecimal> clazz) {
    return Objects.nonNull(val) ? BigDecimal.valueOf(val) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
