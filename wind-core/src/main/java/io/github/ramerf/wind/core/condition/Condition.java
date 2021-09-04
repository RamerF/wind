package io.github.ramerf.wind.core.condition;

import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;

/**
 * 条件对象.
 *
 * @param <POJO> pojo对象
 * @param <CONDITION> 当前对象
 * @since 2020.01.06
 * @author ramer
 */
public interface Condition<POJO, CONDITION extends Condition<POJO, CONDITION>> {
  QueryEntityMetaData<POJO> getQueryEntityMetaData();

  String getString();

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

  default CONDITION eq(@Nonnull final Field field, final Object value) {
    return eq(true, field, value);
  }

  CONDITION eq(final boolean condition, @Nonnull final Field field, final Object value);

  default CONDITION in(@Nonnull Field field, @Nonnull Collection<?> values) {
    return in(true, field, values);
  }

  CONDITION in(boolean condition, @Nonnull Field field, @Nonnull Collection<?> values);

  CONDITION and(final String sql);

  CONDITION or(final String sql);

  /** 拼接逻辑未删除条件,如果不支持逻辑删除,不执行操作. */
  CONDITION appendLogicNotDelete();

  /** 属性匹配模式 */
  enum MatchPattern {
    /** = */
    EQUAL("="),
    /** &lt;&gt; */
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

  enum SqlOperator {
    /** 点. */
    DOT("."),
    /** 逗号. */
    COMMA(","),
    /** 问号. */
    QUESTION_MARK("?"),
    /** 百分号. */
    PERCENT("%"),
    /** 星号. */
    WILDCARD("*"),
    /** 等于. */
    EQUAL("="),
    EQUAL_FORMAT(" %s=%s "),

    /** 引号. */
    QUOTE("'"),
    QUOTE_FORMAT("'%s'"),

    /** 圆括号(小括号). */
    LEFT_PARENTHESIS("("),
    RIGHT_PARENTHESIS(")"),
    PARENTHESIS_FORMAT("(%s)"),

    /** 花括号(大括号). */
    LEFT_BRACE("{"),
    RIGHT_BRACE("}"),
    BRACE_FORMAT("{%s}"),

    /** 方括号(中括号) */
    LEFT_SQUARE_BRACKETS("["),
    RIGHT_SQUARE_BRACKETS("["),
    SQUARE_BRACKETS_FORMAT("[%s]"),

    INSERT_INTO("insert into "),

    VALUES(" values"),

    WHERE(" where "),

    AS(" as "),

    AND(" and "),

    ORDER_BY(" order by "),

    GROUP_BY(" group by "),

    OR(" or "),
    ;

    final String operator;

    SqlOperator(final String operator) {
      this.operator = operator;
    }

    public String operator() {
      return this.operator;
    }

    public String format(final Object... string) {
      return String.format(this.operator, string);
    }

    @Override
    public String toString() {
      return this.operator;
    }
  }
}
