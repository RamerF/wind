package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;

/**
 * The interface Condition.
 *
 * <p>TODO 如果要实现完整的连表查询,考虑在该类添加join方法
 *
 * @param <T> the type parameter
 * @since 2020.01.06
 * @author Tang Xiaofeng
 */
public interface ICondition<T extends AbstractEntityPoJo<T, ?>> extends Predicate<T> {

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @return the Condition
   * @see Condition#defaultConstructor()
   */
  default ICondition<T> condition() {
    return condition(false);
  }

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @param genAlia 是否生成新的表别名,用于子查询时传true
   * @return the Condition
   */
  ICondition<T> condition(final boolean genAlia);

  /**
   * 获取占位符值.<br>
   *
   * @param startIndex 填充参数的起始索引,null时从0开始
   * @return 占位符对应的值 values
   */
  List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex);

  /**
   * 获取所有原始值,生成缓存key时会用到.
   *
   * @return the value string
   */
  List<Object> getOriginValues();

  /**
   * 是否为空,true:不包含任何条件.
   *
   * @return the boolean
   */
  boolean isEmpty();

  ICondition<T> eq(@Nonnull final Field field, final Object value);

  ICondition<T> eq(final boolean condition, @Nonnull final Field field, final Object value);

  ICondition<T> in(@Nonnull Field field, @Nonnull Collection<?> values);

  ICondition<T> in(boolean condition, @Nonnull Field field, @Nonnull Collection<?> values);

  /** 属性匹配模式 */
  enum MatchPattern {
    /** = */
    EQUAL("="),
    /** != */
    NOT_EQUAL("!="),
    /** &gt; */
    GREATER(">"),
    /** &gt;= */
    GE(">="),
    /** &lt; */
    LESS("<"),
    /** &lt;= */
    LE("<="),
    LIKE_PLAIN(" LIKE %s "),
    /** LIKE %criteria% */
    LIKE(" LIKE '%%%s%%'"),
    /** LIKE %criteria */
    LIKE_LEFT(" LIKE '%%%s'"),
    /** LIKE criteria% */
    LIKE_RIGHT(" LIKE '%s%%'"),
    /** NOT LIKE %criteria% */
    NOT_LIKE_PLAIN(" NOT LIKE %s "),
    NOT_LIKE(" NOT LIKE '%%%s%%'"),
    /** BETWEEN start AND end */
    BETWEEN(" BETWEEN %s and %s"),
    /** NOT BETWEEN start AND end */
    NOT_BETWEEN(" NOT BETWEEN %s and %s"),
    /** IS NULL */
    IS_NULL(" IS NULL"),
    /** IS NOT NULL */
    IS_NOT_NULL(" IS NOT NULL"),
    /** EXISTS */
    EXISTS(" EXISTS(%s)"),
    /** NOT EXISTS */
    NOT_EXISTS(" NOT EXISTS(%s)"),
    /** IN () */
    IN(" IN (%s)"),
    /** NOT IN () */
    NOT_IN(" NOT IN (%s)"),
    /** &gt;ANY() */
    GREATER_ANY(" >ANY(%s)"),
    /** &gt;=ANY() */
    GE_ANY(" >=ANY(%s)"),
    /** &lt;ANY() */
    LESS_ANY(" <ANY(%s)"),
    /** &lt;=ANY() */
    LE_ANY(" <=ANY(%s)");

    final String operator;

    public String operator() {
      return operator;
    }

    MatchPattern(final String operator) {
      this.operator = operator;
    }
  }
}
