package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.function.*;
import io.github.ramerf.wind.core.function.IFunction;
import java.math.BigDecimal;
import java.util.Collection;
import javax.annotation.Nonnull;
import lombok.Getter;

/**
 * @author ramer
 * @since 24/07/2021
 */
public class StringCnds<T> extends AbstractCnd<T, StringCnds<T>, StringCondition<T>>
    implements IStringCondition<T, StringCnds<T>> {
  @Getter private Class<T> clazz;
  @Getter private QueryColumn<T> queryColumn;
  @Getter private StringCondition<T> condition;

  private StringCnds() {}

  public static <T> StringCnds<T> of(final Class<T> clazz) {
    final StringCnds<T> cnds = new StringCnds<>();
    cnds.clazz = clazz;
    cnds.queryColumn = QueryColumn.of(clazz);
    cnds.condition = StringCondition.of(clazz);
    return cnds;
  }

  public static <T> StringCnds<T> of(
      @Nonnull final Class<T> clazz, @Nonnull final StringCondition<T> condition) {
    final StringCnds<T> cnds = new StringCnds<>();
    cnds.clazz = clazz;
    cnds.queryColumn = QueryColumn.of(clazz);
    cnds.condition = condition;
    return cnds;
  }

  public static <T> StringCnds<T> of(
      @Nonnull final Class<T> cls,
      @Nonnull final QueryColumn<T> queryColumn,
      @Nonnull final StringCondition<T> condition) {
    final StringCnds<T> cnds = new StringCnds<>();
    cnds.clazz = cls;
    cnds.queryColumn = queryColumn;
    cnds.condition = condition;
    return cnds;
  }

  public StringCnds<T> col(IFunction<T, ?> field) {
    queryColumn.col(field);
    return this;
  }

  /**
   * 查询列.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public StringCnds<T> col(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, null);
  }

  /**
   * sql函数.
   *
   * @see BaseSqlFunction
   * @see CaseWhenSqlFunction
   */
  public StringCnds<T> col(final SqlFunction sqlFunction) {
    return col(sqlFunction.string());
  }

  /**
   * 自定义查询表达式.
   *
   * <p>示例:col("id,case sex when 1 then '男' when 2 then '女' else '未知' end")
   *
   * @param sql 查询列表达式
   * @return the query column
   */
  public StringCnds<T> col(@Nonnull final String sql) {
    queryColumn.col(sql);
    return this;
  }

  /**
   * 对指定字段做统计.
   *
   * @param function the function
   * @return the query column
   */
  public StringCnds<T> count(final IFunction<T, ?> function) {
    return count(function, null);
  }

  /**
   * 对指定字段做统计.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public StringCnds<T> count(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.COUNT);
  }

  /**
   * 对指定字段求和.
   *
   * @param function the function
   * @return the query column
   * @see #sum(IFunction, String) #sum(IFunction, String)
   */
  public StringCnds<T> sum(final IFunction<T, ?> function) {
    return sum(function, null);
  }

  /**
   * 对指定字段求和.不确定返回类型的情况下使用{@link BigDecimal}
   *
   * <pre>
   * <b>注意:该列的返回类型与数据库对应关系</b>
   * <b>java           jdbc</b>
   * BigDecimal     bigint/numeric/decimal
   * Double         double/float
   *
   * </pre>
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public StringCnds<T> sum(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.SUM);
  }

  /** 对指定表达式求和,比如sum(case when...) */
  public StringCnds<T> sum(final SqlFunction sqlFunction) {
    return col(AggregateSqlFunction.SUM.string(sqlFunction.string()));
  }

  /**
   * 对指定字段求最大值.
   *
   * @param function the function
   * @return the query column
   */
  public StringCnds<T> max(final IFunction<T, ?> function) {
    return max(function, null);
  }

  /**
   * 对指定字段求最大值.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public StringCnds<T> max(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.MAX);
  }

  /**
   * 对指定字段求最小值.
   *
   * @param function the function
   * @return the query column
   */
  public StringCnds<T> min(final IFunction<T, ?> function) {
    return min(function, null);
  }

  /**
   * 对指定字段求最小值.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public StringCnds<T> min(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.MIN);
  }

  /** 添加查询对象(列/聚合函数). */
  public StringCnds<T> col(final IFunction<T, ?> function, final SqlFunction sqlFunction) {
    return col(function, null, sqlFunction);
  }

  /** 添加查询对象(列/聚合函数). */
  public StringCnds<T> col(
      final IFunction<T, ?> function, final String alia, final SqlFunction sqlFunction) {
    queryColumn.col(function, alia, sqlFunction);
    return this;
  }

  @Override
  public StringCnds<T> eq(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.eq(true, column, value);
    return this;
  }

  @Override
  public StringCnds<T> ne(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.ne(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> gt(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.gt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> ge(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.ge(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> lt(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.gt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> le(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.le(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> like(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.like(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> notLike(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.notLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> between(
      final boolean cond,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    condition.between(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> notBetween(
      final boolean cond,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    condition.notBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> isNull(final boolean cond, @Nonnull final String column) {
    condition.isNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> isNotNull(final boolean cond, @Nonnull final String column) {
    condition.isNotNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> in(
      final boolean cond, @Nonnull final String column, @Nonnull final Collection<?> values) {
    condition.in(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> notIn(
      final boolean cond, @Nonnull final String column, @Nonnull final Collection<?> values) {
    condition.notIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> orEq(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orEq(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orNe(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orNe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orGt(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orGt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orGe(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orGe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLt(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orLt(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLe(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orLe(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orLike(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orNotLike(
      final boolean cond, @Nonnull final String column, @Nonnull final Object value) {
    condition.orNotLike(cond, column, value);
    return this;
  }

  @Override
  public StringCnds<T> orBetween(
      final boolean cond,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    condition.orBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> orNotBetween(
      final boolean cond,
      @Nonnull final String column,
      @Nonnull final Object start,
      @Nonnull final Object end) {
    condition.orNotBetween(cond, column, start, end);
    return this;
  }

  @Override
  public StringCnds<T> orIsNull(final boolean cond, @Nonnull final String column) {
    condition.orIsNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> orIsNotNull(final boolean cond, @Nonnull final String column) {
    condition.orIsNotNull(cond, column);
    return this;
  }

  @Override
  public StringCnds<T> orIn(
      final boolean cond, @Nonnull final String column, @Nonnull final Collection<?> values) {
    condition.orIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> orNotIn(
      final boolean cond, @Nonnull final String column, @Nonnull final Collection<?> values) {
    condition.orNotIn(cond, column, values);
    return this;
  }

  @Override
  public StringCnds<T> and(@Nonnull StringConditionGroup<T> group) {
    condition.and(group);
    return this;
  }

  @Override
  public StringCnds<T> or(@Nonnull StringConditionGroup<T> group) {
    condition.or(group);
    return this;
  }

  @Override
  public StringCnds<T> and(final String column, final String operator, final Object value) {
    condition.and(column, operator, value);
    return this;
  }

  @Override
  public StringCnds<T> or(final String column, final String operator, final Object value) {
    condition.or(column, operator, value);
    return this;
  }

  @Override
  public StringCnds<T> groupBy(@Nonnull final String column) {
    condition.groupBy(column);
    return this;
  }
}
