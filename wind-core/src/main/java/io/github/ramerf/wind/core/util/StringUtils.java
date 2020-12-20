package io.github.ramerf.wind.core.util;

import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.*;
import lombok.extern.slf4j.Slf4j;

/**
 * The type String utils.
 *
 * @author Tang Xiaofeng
 * @since 2019 /12/27
 */
@Slf4j
@SuppressWarnings("unused")
public class StringUtils {

  /**
   * Is empty boolean.
   *
   * @param str the str
   * @return the boolean
   * @see #hasText(String)
   */
  public static boolean isEmpty(final String str) {
    return (str == null || str.isEmpty());
  }

  /**
   * Non empty boolean.
   *
   * @param str the str
   * @return the boolean
   */
  public static boolean nonEmpty(final String str) {
    return !isEmpty(str);
  }

  /**
   * 首字母转大写
   *
   * @param string the string
   * @return the string
   */
  public static String firstUppercase(final String string) {
    char[] chars = string.toCharArray();
    if (chars[0] >= 97 && chars[0] <= 122) {
      chars[0] -= 32;
    }
    return String.valueOf(chars);
  }

  /**
   * 首字母转小写
   *
   * @param string the string
   * @return the string
   */
  public static String firstLowercase(final String string) {
    char[] chars = string.toCharArray();
    if (chars[0] >= 65 && chars[0] <= 90) {
      chars[0] += 32;
    }
    return String.valueOf(chars);
  }

  /**
   * 驼峰转下划线.
   *
   * @param camelString the camel string
   * @return the string
   */
  public static String camelToUnderline(final String camelString) {
    final char[] chars = camelString.toCharArray();
    StringBuilder stringBuilder = new StringBuilder();
    IntStream.range(0, chars.length)
        .mapToObj(i -> String.valueOf(chars[i]))
        .forEach(
            s -> {
              if (s.matches("[A-Z]")
                  && stringBuilder.length() > 0
                  && stringBuilder.charAt(stringBuilder.length() - 1) != '_') {
                stringBuilder.append("_");
              }
              stringBuilder.append(s.toLowerCase());
            });
    return stringBuilder.toString();
  }

  /**
   * 给定字符串以逗号{@code ,}分割,最后一个空白字符将会被删除.
   *
   * @return the list
   * @see StringUtils#splitToLong(String, String)
   */
  public static List<Long> splitToLong(final String str) {
    return splitToLong(str, ",");
  }

  /**
   * 给定字符串以{@code delim}分割,最后一个空白字符将会被删除.
   *
   * @param str the str
   * @param delim 分隔符
   * @return the list
   * @see StringUtils#split(String, String, Function)
   */
  public static List<Long> splitToLong(final String str, final String delim) {
    return split(str, delim, Long::valueOf);
  }

  /**
   * 给定字符串以{@code delim}分割,最后一个空白字符将会被删除.
   *
   * @param <T> 返回集合元素类型
   * @param str the str
   * @param delim 分隔符
   * @param function 转换函数表达式,返回值即为集合元素<br>
   *     例如: {@code s->Long.valueOf(s)}
   * @return the list
   */
  public static <T> List<T> split(
      final String str, final String delim, Function<String, T> function) {
    if (isEmpty(str)) {
      return Collections.emptyList();
    }
    return Stream.of(str.split(delim)).map(function).collect(Collectors.toList());
  }

  /**
   * 检测是否包含非空白字符.
   *
   * @param str the {@code String} to check (may be {@code null})
   * @return {@code true} if the {@code String} is not {@code null}, its length is greater than 0,
   *     and it does not contain whitespace only
   * @see Character#isWhitespace(char)
   *     Character#isWhitespace(char)Character#isWhitespace(char)Character#isWhitespace(char)
   */
  public static boolean hasText(final String str) {
    return (str != null && !str.isEmpty() && containsText(str));
  }

  private static boolean containsText(final CharSequence str) {
    int strLen = str.length();
    for (int i = 0; i < strLen; i++) {
      if (!Character.isWhitespace(str.charAt(i))) {
        return true;
      }
    }
    return false;
  }

  /**
   * 检测是否有长度,仅仅包含空白字符的字符串也会返回true.
   *
   * @param str the {@code String} to check (may be {@code null})
   * @return {@code true} if the {@code String} is not {@code null} and has length
   */
  public static boolean hasLength(final String str) {
    return (str != null && !str.isEmpty());
  }

  /**
   * 去除前后空白字符(空白字符定义 {@link Character#isWhitespace(char)}).
   *
   * @param str the {@code String} to check
   * @return the trimmed {@code String}
   * @see java.lang.Character#isWhitespace java.lang.Character#isWhitespace
   *     java.lang.Character#isWhitespacejava.lang.Character#isWhitespace
   */
  public static String trimWhitespace(String str) {
    if (!hasLength(str)) {
      return str;
    }
    int beginIndex = 0;
    int endIndex = str.length() - 1;
    while (beginIndex <= endIndex && Character.isWhitespace(str.charAt(beginIndex))) {
      beginIndex++;
    }
    while (endIndex > beginIndex && Character.isWhitespace(str.charAt(endIndex))) {
      endIndex--;
    }
    return str.substring(beginIndex, endIndex + 1);
  }

  /**
   * 去除所有空白字符(空白字符定义 {@link Character#isWhitespace(char)}).
   *
   * @param str the {@code String} to check
   * @return the trimmed {@code String}
   * @see java.lang.Character#isWhitespace java.lang.Character#isWhitespace
   *     java.lang.Character#isWhitespacejava.lang.Character#isWhitespace
   */
  public static String trimAllWhitespace(final String str) {
    if (!hasLength(str)) {
      return str;
    }
    int len = str.length();
    StringBuilder sb = new StringBuilder(str.length());
    for (int i = 0; i < len; i++) {
      char c = str.charAt(i);
      if (!Character.isWhitespace(c)) {
        sb.append(c);
      }
    }
    return sb.toString();
  }

  public static void doIfNonEmpty(final String string, final Consumer<String> consumer) {
    if (nonEmpty(string)) {
      consumer.accept(string);
    }
  }

  /**
   * The entry point of application.
   *
   * @param args the input arguments
   */
  public static void main(String[] args) {
    log.info("main:firstUppercase[{}]", firstUppercase("firstUppercase"));
    log.info("main:firstLowercase[{}]", firstLowercase("Uppercase"));
    log.info("main:split[{}]", split(",Upper,case,", ",", Function.identity()));
    log.info("main:splitToLong[{}]", splitToLong("1,2", ","));
    log.info("main:hasText[{}]", hasText(" "));
    log.info("main:hasLength[{}]", hasLength(" "));
    log.info("main:trimAllWhitespace[{}]", trimWhitespace(" Q W E "));
    log.info("main:camelToUnderline[{}]", camelToUnderline("bigId"));
    log.info("main:camelToUnderline[{}]", camelToUnderline("bigDecimal_Id"));
  }
}
