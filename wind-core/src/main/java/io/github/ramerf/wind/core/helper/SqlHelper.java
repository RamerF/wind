package io.github.ramerf.wind.core.helper;

import java.util.*;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.*;

/**
 * The type Sql helper.
 *
 * @author ramer
 * @since 2020 /1/13
 */
@Slf4j
public class SqlHelper {

  /**
   * 将值转换成sql值,始终返回?.
   *
   * @param value the value
   * @return ?
   */
  public static String toPreFormatSqlVal(final Object value) {
    return "?";
  }

  /**
   * 将值转换成Sql字符串.
   *
   * @param value the value
   * @return the string
   */
  public static String toSqlString(final Object value) {
    if (value == null) {
      return "null";
    }
    if (value instanceof String) {
      String val = ((String) value);
      final String quote = "'";
      if (val.contains(quote)) {
        val = val.replaceAll(quote, "''");
      }
      return QUOTE_FORMAT.format(val);
    }
    if (value instanceof Date) {
      /// return QUOTE_FORMAT.format(
      //     LocalDateTime.ofInstant(((Date) value).toInstant(), Constant.DEFAULT_ZONE).toString());
      return QUOTE_FORMAT.format(((Date) value).toInstant().toString());
    }
    if (Collection.class.isAssignableFrom(value.getClass())) {
      return QUOTE_FORMAT.format(
          BRACE_FORMAT.format(
              ((Collection<?>) value)
                  .stream()
                      .map(SqlHelper::toSqlString)
                      .reduce((a, b) -> String.join(COMMA.operator(), a, b))
                      .orElse(value.toString())));
    }
    if (value.getClass().isArray()) {
      if (value instanceof String[]) {
        return QUOTE_FORMAT.format(
            LEFT_BRACE
                .operator()
                .concat(
                    Arrays.stream((String[]) value)
                        .reduce((a, b) -> String.join(COMMA.operator(), a, b))
                        .orElse(value.toString()))
                .concat(RIGHT_BRACE.operator()));
      }
      String str =
          Arrays.stream((Object[]) value)
              .map(SqlHelper::toSqlString)
              .reduce((a, b) -> String.join(COMMA.operator(), a, b))
              .orElse(value.toString());
      return QUOTE_FORMAT.format(LEFT_BRACE.operator().concat(str).concat(RIGHT_BRACE.operator()));
    }
    return String.valueOf(value);
  }
}
