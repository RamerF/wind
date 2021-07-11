package io.github.ramerf.wind.core.condition.function;

import io.github.ramerf.wind.core.helper.SqlHelper;
import javax.annotation.Nonnull;

/**
 * sql function.
 *
 * @author ramer
 * @since 2020 /4/29
 */
public class CaseWhenSqlFunction extends BaseSqlFunction {

  public static <T, R> CaseWhenSqlFunction caseWhen(final String caseWhen) {
    final CaseWhenSqlFunction sqlFunction = new CaseWhenSqlFunction();
    sqlFunction.sqlBuilder.append("case when ").append(caseWhen);
    return sqlFunction;
  }

  public CaseWhenSqlFunction cases(final String cases) {
    sqlBuilder.append(" case ").append(cases);
    return this;
  }

  public CaseWhenSqlFunction when(final String when) {
    sqlBuilder.append(" when ").append(when);
    return this;
  }

  public CaseWhenSqlFunction and(final String and) {
    sqlBuilder.append(" and ").append(and);
    return this;
  }

  public CaseWhenSqlFunction then(final Object then) {
    sqlBuilder.append(" then ").append(SqlHelper.toSqlString(then));
    return this;
  }

  public CaseWhenSqlFunction elses(final Object elses) {
    sqlBuilder.append(" else ").append(SqlHelper.toSqlString(elses));
    return this;
  }

  public CaseWhenSqlFunction end() {
    sqlBuilder.append(" end");
    return this;
  }

  public CaseWhenSqlFunction end(@Nonnull final String alia) {
    sqlBuilder.insert(0, "(").append(" end)").append("as ").append(alia);
    return this;
  }
}
