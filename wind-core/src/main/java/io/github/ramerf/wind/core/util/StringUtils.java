package io.github.ramerf.wind.core.util;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.*;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;

/**
 * The type String utils.
 *
 * @author ramer
 * @since 2019 /12/27
 */
@Slf4j
@SuppressWarnings("unused")
public class StringUtils {
  private static final String FOLDER_SEPARATOR = "/";
  private static final String WINDOWS_FOLDER_SEPARATOR = "\\";
  private static final char PATH_SEPARATOR = '/';
  private static final String TOP_PATH = "..";
  private static final String CURRENT_PATH = ".";
  private static final char PACKAGE_SEPARATOR = '.';

  /**
   * 字符串为null或者长度为0.
   *
   * @see #hasText(String)
   */
  public static boolean isEmpty(final String str) {
    return (str == null || str.isEmpty());
  }

  /** 字符串长度大于0. */
  public static boolean nonEmpty(final String str) {
    return !isEmpty(str);
  }

  /** 首字母转大写 */
  public static String firstUppercase(final String string) {
    char[] chars = string.toCharArray();
    if (chars[0] >= 97 && chars[0] <= 122) {
      chars[0] -= 32;
    }
    return String.valueOf(chars);
  }

  /** 首字母转小写 */
  public static String firstLowercase(final String string) {
    char[] chars = string.toCharArray();
    if (chars[0] >= 65 && chars[0] <= 90) {
      chars[0] += 32;
    }
    return String.valueOf(chars);
  }

  /** 驼峰转下划线. */
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

  /** 短横线转驼峰. */
  public static String dashToCamel(final String camelString) {
    if (camelString == null || !camelString.contains("-")) {
      return camelString;
    }
    StringBuffer buffer = new StringBuffer();
    Matcher matcher = Pattern.compile("(-)([a-z])").matcher(camelString);
    while (matcher.find()) {
      matcher.appendReplacement(
          buffer, matcher.group().replaceAll("-", "").toUpperCase(Locale.ENGLISH));
    }
    matcher.appendTail(buffer);
    return buffer.toString();
  }

  /**
   * 给定字符串以逗号{@code ,}分割,最后一个空白字符将会被删除.
   *
   * @see StringUtils#splitToLong(String, String)
   */
  public static List<Long> splitToLong(final String str) {
    return splitToLong(str, ",");
  }

  /**
   * 给定字符串以{@code delim}分割,最后一个空白字符将会被删除.
   *
   * @param delim 分隔符
   * @see StringUtils#split(String, String, Function)
   */
  public static List<Long> splitToLong(final String str, final String delim) {
    return split(str, delim, Long::valueOf);
  }

  /**
   * 给定字符串以{@code delim}分割,最后一个空白字符将会被删除.
   *
   * @param <T> 返回集合元素类型
   * @param delim 分隔符
   * @param function 转换函数表达式,返回值即为集合元素<br>
   *     例如: {@code s->Long.valueOf(s)}
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
   * @return 仅当包含非空白字符时返回true
   * @see Character#isWhitespace(char)
   */
  public static boolean hasText(final String str) {
    return (str != null && !str.isEmpty() && containsText(str));
  }

  private static boolean containsText(@Nonnull final CharSequence str) {
    int strLen = str.length();
    for (int i = 0; i < strLen; i++) {
      if (!Character.isWhitespace(str.charAt(i))) {
        return true;
      }
    }
    return false;
  }

  /**
   * 去除前后空白字符(空白字符定义 {@link Character#isWhitespace(char)}).
   *
   * @param str the {@code String} to check
   * @return the trimmed {@code String}
   * @see java.lang.Character#isWhitespace
   */
  public static String trimWhitespace(String str) {
    if (isEmpty(str)) {
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
   * @see java.lang.Character#isWhitespace
   */
  public static String trimAllWhitespace(final String str) {
    if (isEmpty(str)) {
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

  /** 使用指定分隔符拆分字符串,忽略空串,去除前后空格 */
  public static String[] tokenizeToStringArray(@Nullable String str, String delimiters) {
    return tokenizeToStringArray(str, delimiters, true, true);
  }

  /**
   * 使用指定分隔符拆分字符串
   *
   * @param delimiters 分隔符,可以是多个
   * @param trim 拆分后的子串是否执行{@link String#trim()}
   * @param ignoreEmpty 是否忽略空串
   */
  public static String[] tokenizeToStringArray(
      @Nullable String str, String delimiters, boolean trim, boolean ignoreEmpty) {
    if (str == null) {
      return new String[0];
    }

    StringTokenizer st = new StringTokenizer(str, delimiters);
    List<String> tokens = new ArrayList<>();
    while (st.hasMoreTokens()) {
      String token = st.nextToken();
      if (trim) {
        token = token.trim();
      }
      if (!ignoreEmpty || token.length() > 0) {
        tokens.add(token);
      }
    }
    return tokens.toArray(new String[0]);
  }

  /** 转换为资源路径 */
  public static String convertToResourcePath(final String path) {
    Asserts.notNull(path, "path must not be null");
    return path.replace(PACKAGE_SEPARATOR, PATH_SEPARATOR);
  }

  public static void main(String[] args) {
    log.info("main:firstUppercase[{}]", firstUppercase("firstUppercase"));
    log.info("main:firstLowercase[{}]", firstLowercase("Uppercase"));
    log.info("main:split[{}]", split(",Upper,case,", ",", Function.identity()));
    log.info("main:splitToLong[{}]", splitToLong("1,2", ","));
    log.info("main:hasText[{}]", hasText(" "));
    log.info("main:isEmpty[{}]", isEmpty(" "));
    log.info("main:trimAllWhitespace[{}]", trimWhitespace(" Q W E "));
    log.info("main:camelToUnderline[{}]", camelToUnderline("bigId"));
    log.info("main:camelToUnderline[{}]", camelToUnderline("bigDecimal_Id"));
  }
}
