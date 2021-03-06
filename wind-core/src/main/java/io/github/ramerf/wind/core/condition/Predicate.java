package io.github.ramerf.wind.core.condition;

/**
 * @author ramer
 * @since 2020/1/5
 */
public interface Predicate<T> extends QueryEntity<T> {
  /**
   * 获取条件sql.
   *
   * @return the string
   */
  @Override
  String getString();

  enum SqlOperator {
    /** 点. */
    DOT("."),
    /** 逗号. */
    SEMICOLON(","),
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

    protected final String operator;

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
