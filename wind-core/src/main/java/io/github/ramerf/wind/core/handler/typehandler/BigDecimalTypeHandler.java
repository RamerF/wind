package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.util.Objects;
import javax.annotation.Nonnull;

/**
 * {@literal java:BigDecimal <=> jdbc:Double}.
 *
 * @author Tang Xiaofeng
 * @since 2020/3/4
 */
public class BigDecimalTypeHandler implements ITypeHandler<BigDecimal, Double> {
  @Override
  public Double convertToJdbc(
      BigDecimal val, final Field field, @Nonnull final PreparedStatement ps) {
    return Objects.nonNull(val) ? val.doubleValue() : null;
  }

  @Override
  public BigDecimal convertFromJdbc(
      final Double val, final Object defaultValue, final Field field) {
    return Objects.nonNull(val) ? BigDecimal.valueOf(val) : null;
  }

  @Override
  public String getJdbcType(@Nonnull final Field field) {
    return null;
  }
}
