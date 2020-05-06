package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.AbstractEntity;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
public interface Predicate<T extends AbstractEntity> extends QueryEntity<T> {
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

    /** 圆括号. */
    LEFT_BRACKET("("),
    RIGHT_BRACKET(")"),
    BRACKET_FORMAT("(%s)"),

    /** 花括号. */
    LEFT_BRACE("{"),
    RIGHT_BRACE("}"),
    BRACE_FORMAT("{%s}"),

    INSERT_INTO("INSERT INTO "),

    VALUES(" VALUES"),

    WHERE(" WHERE "),

    AS(" AS "),

    AND(" AND "),

    ORDER_BY(" ORDER BY "),

    GROUP_BY(" GROUP BY "),

    OR(" OR "),
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
