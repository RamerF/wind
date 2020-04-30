package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
@SuppressWarnings("unused")
public interface Predicate<T extends AbstractEntity> extends QueryEntity<T> {
  /**
   * 获取条件sql.
   *
   * @return the string
   */
  String getCondition();

  enum SqlOperator {
    /** 点. */
    DOT("."),
    /** 逗号. */
    SEMICOLON(","),
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

    private final String operator;

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
