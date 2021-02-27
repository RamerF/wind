package io.github.ramerf.wind.core.condition;

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
 * <p>TODO POST 如果要实现完整的连表查询,考虑在该类添加join方法
 *
 * @param <T> the type parameter
 * @since 2020.01.06
 * @author ramer
 */
public interface Condition<T> extends Predicate<T> {

  // TODO WARN 使用ConditionGroup实现and/or后,删除<br>
  /**
   * 创建一个空的条件,包含表信息.
   *
   * @return the Condition
   * @see LambdaCondition#getInstance(QueryColumn)
   */
  @Deprecated
  default Condition<T> condition() {
    return condition(false);
  }

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @param genAlia 是否生成新的表别名,用于子查询时传true
   * @return the Condition
   */
  @Deprecated
  Condition<T> condition(final boolean genAlia);

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

  Condition<T> eq(@Nonnull final Field field, final Object value);

  Condition<T> eq(final boolean condition, @Nonnull final Field field, final Object value);

  Condition<T> in(@Nonnull Field field, @Nonnull Collection<?> values);

  Condition<T> in(boolean condition, @Nonnull Field field, @Nonnull Collection<?> values);

  /** 拼接逻辑未删除条件,如果不支持逻辑删除,不执行操作. */
  Condition<T> appendLogicNotDelete();

  /** 属性匹配模式 */
  enum MatchPattern {
    /** = */
    EQUAL("="),
    /** != */
    NOT_EQUAL("<>"),
    /** &gt; */
    GREATER(">"),
    /** &gt;= */
    GE(">="),
    /** &lt; */
    LESS("<"),
    /** &lt;= */
    LE("<="),
    LIKE_PLAIN(" like %s "),
    /** LIKE %criteria% */
    LIKE(" like '%%%s%%'"),
    /** LIKE %criteria */
    LIKE_LEFT(" like '%%%s'"),
    /** LIKE criteria% */
    LIKE_RIGHT(" like '%s%%'"),
    /** NOT LIKE %criteria% */
    NOT_LIKE_PLAIN(" not like %s "),
    NOT_LIKE(" not like '%%%s%%'"),
    /** BETWEEN start AND end */
    BETWEEN(" between %s and %s"),
    /** NOT BETWEEN start AND end */
    NOT_BETWEEN(" not between %s and %s"),
    /** IS NULL */
    IS_NULL(" is null"),
    /** IS NOT NULL */
    IS_NOT_NULL(" is not null"),
    /** EXISTS */
    EXISTS(" exists(%s)"),
    /** NOT EXISTS */
    NOT_EXISTS(" not exists(%s)"),
    /** IN () */
    IN(" in (%s)"),
    /** NOT IN () */
    NOT_IN(" not in (%s)"),
    /** &gt;ANY() */
    GREATER_ANY(" >any(%s)"),
    /** &gt;=ANY() */
    GE_ANY(" >=any(%s)"),
    /** &lt;ANY() */
    LESS_ANY(" <any(%s)"),
    /** &lt;=ANY() */
    LE_ANY(" <=any(%s)");

    final String operator;

    public String operator() {
      return operator;
    }

    MatchPattern(final String operator) {
      this.operator = operator;
    }
  }
}
