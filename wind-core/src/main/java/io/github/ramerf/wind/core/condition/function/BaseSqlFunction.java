package io.github.ramerf.wind.core.condition.function;

/**
 * sql function.
 *
 * @author ramer
 * @since 2020 /4/29
 */
public abstract class BaseSqlFunction implements SqlFunction {
  protected final StringBuilder sqlBuilder = new StringBuilder();

  @Override
  public String string(final String... str) {
    return sqlBuilder.toString();
  }
}
