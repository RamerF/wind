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
   * 将值转换成Sql字符串,仅用于<code>RedisCache</code> key前缀生成.
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
    if (List.class.isAssignableFrom(value.getClass())) {
      // 数组拼接为: '{name1,name2}' 或使用函数 string_to_array('name1,name2', ',')
      // 目前不支持多数据库,考虑到兼容性,不使用函数
      return QUOTE_FORMAT.format(
          BRACE_FORMAT.format(
              ((List<?>) value)
                  .stream()
                      .map(Object::toString)
                      .reduce((a, b) -> String.join(COMMA.operator(), a, b))
                      .orElse(value.toString())));
    }
    if (value instanceof Collection) {
      // 不知道当时写这个是干什么的 0_0
      return ((Collection<?>) value)
          .stream()
              .map(SqlHelper::toSqlString)
              .filter(Objects::nonNull)
              .map(Object::toString)
              .reduce((a, b) -> String.join(COMMA.operator(), a, b))
              .orElse(value.toString());
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

  /**
   * 优化查询字符串,目前主要是更新返回类型推断查询字段,只会优化单表查询.
   *
   * @param <R> the type parameter
   * @param old 原字符串
   * @param clazz 返回类型
   * @return string string
   */
  public static <R> String optimizeQueryString(final String old, final Class<R> clazz) {
    // 可以改为如果返回对象和查询对象不同时,根据原始list string匹配字段
    /// 根据返回对象推断查询字段,返回对象可能包含数据库不存在的字段,禁用
    //    if (log.isDebugEnabled()) {
    //      log.debug("optimizeQueryString:start optimize query string[{}]", old);
    //    }
    //    if (AbstractEntityPoJo.class.isAssignableFrom(clazz) || !old.matches("\\w+\\.\\*")) {
    //      return old;
    //    }
    //    return Optional.ofNullable(QUERY_CLAZZ_FIELD.get(clazz)).map(Reference::get).orElseGet(()
    // -> {
    //      final String optimizedString = BeanUtils.getPrivateFields(clazz,
    // true).stream().filter(str -> !str.contains("."))
    //          .map(StringUtils::camelToUnderline).reduce((a, b) ->
    // String.join(SEMICOLON.operator(), a, b))
    //          .orElse(old);
    //      log.info("optimizeQueryString:after optimized[{}]", optimizedString);
    //      QUERY_CLAZZ_FIELD.put(clazz, new WeakReference<>(optimizedString));
    //      return QUERY_CLAZZ_FIELD.get(clazz).get();
    //    });
    return old;
  }
}
