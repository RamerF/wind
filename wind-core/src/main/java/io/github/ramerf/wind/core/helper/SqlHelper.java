package io.github.ramerf.wind.core.helper;

import io.github.ramerf.wind.core.converter.EnumTypeConverter;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.lang.ref.WeakReference;
import java.time.LocalDateTime;
import java.util.*;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;

/**
 * The type Sql helper.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/13
 */
@Slf4j
public class SqlHelper {
  @SuppressWarnings("unused")
  private static final Map<Class<? extends AbstractEntity>, WeakReference<String>>
      QUERY_CLAZZ_FIELD = new HashMap<>();

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
   * 将值转换成Sql字符串.目前仅支持 <code>null,String,Date,List,Collection,InterEnum,[]</code>
   *
   * @param value the value
   * @return the string
   */
  public static String toSqlString(final Object value) {
    if (Objects.isNull(value)) {
      return "null";
    }
    if (value instanceof String) {
      String val = ((String) value);
      if (val.contains("'")) {
        val = val.replaceAll("'", "''");
      }
      return QUOTE_FORMAT.format(val);
    }
    if (value instanceof Date) {
      return QUOTE_FORMAT.format(
          LocalDateTime.ofInstant(((Date) value).toInstant(), Constant.DEFAULT_ZONE).toString());
    }
    if (List.class.isAssignableFrom(value.getClass())) {
      // 数组拼接为: '{name1,name2}' 或使用函数 string_to_array('name1,name2', ',')
      // 目前不支持多数据库,考虑到兼容性,不使用函数
      return QUOTE_FORMAT.format(
          BRACE_FORMAT.format(
              ((List<?>) value)
                  .stream()
                      .map(Object::toString)
                      .reduce((a, b) -> String.join(SEMICOLON.operator(), a, b))
                      .orElse(value.toString())));
    }
    if (value instanceof Collection) {
      // 不知道当时写这个是干什么的 0_0
      return ((Collection<?>) value)
          .stream()
              .map(SqlHelper::toSqlString)
              .filter(Objects::nonNull)
              .map(Object::toString)
              .reduce((a, b) -> String.join(SEMICOLON.operator(), a, b))
              .orElse(value.toString());
    }
    if (InterEnum.class.isAssignableFrom(value.getClass())) {
      return new EnumTypeConverter().convertToJdbc((InterEnum) value).toString();
    }
    if (value.getClass().isArray()) {
      if (value instanceof String[]) {
        return QUOTE_FORMAT.format(
            LEFT_BRACE
                .operator()
                .concat(
                    Arrays.stream((String[]) value)
                        .reduce((a, b) -> String.join(SEMICOLON.operator(), a, b))
                        .orElse(value.toString()))
                .concat(RIGHT_BRACE.operator()));
      }
      String str =
          Arrays.stream((Object[]) value)
              .map(SqlHelper::toSqlString)
              .reduce((a, b) -> String.join(SEMICOLON.operator(), a, b))
              .orElse(value.toString());
      return QUOTE_FORMAT.format(LEFT_BRACE.operator().concat(str).concat(RIGHT_BRACE.operator()));
    }
    return String.valueOf(value);
  }

  /**
   * 打印包含值的sql语句,仅用于调试.<br>
   * 关于: @SneakyThrows,不考虑性能,不想关心失败情况
   *
   * @param sql the sql
   * @param values the values
   */
  @SneakyThrows
  public static void printSqlWithVal(final String sql, final List<Object> values) {
    String sqlWithVal = sql;
    int index = 0;
    while (index < values.size()) {
      sqlWithVal = sqlWithVal.replaceFirst("\\?", SqlHelper.toSqlString(values.get(index++)));
    }
    log.debug("printSqlWithVal:[{}]", sqlWithVal);
  }

  /**
   * 优化查询字符串,目前主要是更新返回类型推断查询字段,只会优化单表查询.
   *
   * @param <R> the type parameter
   * @param old 原字符串
   * @param clazz 返回类型
   * @return string string
   */
  @SuppressWarnings("unused")
  public static <R> String optimizeQueryString(final String old, final Class<R> clazz) {
    /// 目前项目代码使用了很多包含非pojo字段的 response返回,导致推断出来的字段不存在,所以暂时禁用
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
