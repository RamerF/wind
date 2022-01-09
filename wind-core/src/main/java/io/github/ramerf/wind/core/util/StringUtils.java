package io.github.ramerf.wind.core.util;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.*;
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
  private static final String TOP_PATH = "..";
  private static final String CURRENT_PATH = ".";
  private static final char EXTENSION_SEPARATOR = '.';

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

  /**
   * Normalize the path by suppressing sequences like "path/.." and inner simple dots.
   *
   * <p>The result is convenient for path comparison. For other uses, notice that Windows separators
   * ("\") are replaced by simple slashes.
   *
   * @param path the original path
   * @return the normalized path
   */
  public static String cleanPath(String path) {
    if (!hasLength(path)) {
      return path;
    }
    String pathToUse = replace(path, WINDOWS_FOLDER_SEPARATOR, FOLDER_SEPARATOR);

    // Strip prefix from path to analyze, to not treat it as part of the
    // first path element. This is necessary to correctly parse paths like
    // "file:core/../core/io/Resource.class", where the ".." should just
    // strip the first "core" directory while keeping the "file:" prefix.
    int prefixIndex = pathToUse.indexOf(':');
    String prefix = "";
    if (prefixIndex != -1) {
      prefix = pathToUse.substring(0, prefixIndex + 1);
      if (prefix.contains(FOLDER_SEPARATOR)) {
        prefix = "";
      } else {
        pathToUse = pathToUse.substring(prefixIndex + 1);
      }
    }
    if (pathToUse.startsWith(FOLDER_SEPARATOR)) {
      prefix = prefix + FOLDER_SEPARATOR;
      pathToUse = pathToUse.substring(1);
    }

    String[] pathArray = delimitedListToStringArray(pathToUse, FOLDER_SEPARATOR);
    LinkedList<String> pathElements = new LinkedList<>();
    int tops = 0;

    for (int i = pathArray.length - 1; i >= 0; i--) {
      String element = pathArray[i];
      if (CURRENT_PATH.equals(element)) {
        // Points to current directory - drop it.
      } else if (TOP_PATH.equals(element)) {
        // Registering top path found.
        tops++;
      } else {
        if (tops > 0) {
          // Merging path element with element corresponding to top path.
          tops--;
        } else {
          // Normal path element found.
          pathElements.add(0, element);
        }
      }
    }

    // Remaining top paths need to be retained.
    for (int i = 0; i < tops; i++) {
      pathElements.add(0, TOP_PATH);
    }
    // If nothing else left, at least explicitly point to current path.
    if (pathElements.size() == 1
        && "".equals(pathElements.getLast())
        && !prefix.endsWith(FOLDER_SEPARATOR)) {
      pathElements.add(0, CURRENT_PATH);
    }

    return prefix + collectionToDelimitedString(pathElements, FOLDER_SEPARATOR);
  }

  /**
   * Take a {@code String} that is a delimited list and convert it into a {@code String} array.
   *
   * <p>A single {@code delimiter} may consist of more than one character, but it will still be
   * considered as a single delimiter string, rather than as bunch of potential delimiter
   * characters, in contrast to {@link #tokenizeToStringArray}.
   *
   * @param str the input {@code String} (potentially {@code null} or empty)
   * @param delimiter the delimiter between elements (this is a single delimiter, rather than a
   *     bunch individual delimiter characters)
   * @return an array of the tokens in the list
   * @see #tokenizeToStringArray
   */
  public static String[] delimitedListToStringArray(
      @Nullable String str, @Nullable String delimiter) {
    return delimitedListToStringArray(str, delimiter, null);
  }

  /**
   * Take a {@code String} that is a delimited list and convert it into a {@code String} array.
   *
   * <p>A single {@code delimiter} may consist of more than one character, but it will still be
   * considered as a single delimiter string, rather than as bunch of potential delimiter
   * characters, in contrast to {@link #tokenizeToStringArray}.
   *
   * @param str the input {@code String} (potentially {@code null} or empty)
   * @param delimiter the delimiter between elements (this is a single delimiter, rather than a
   *     bunch individual delimiter characters)
   * @param charsToDelete a set of characters to delete; useful for deleting unwanted line breaks:
   *     e.g. "\r\n\f" will delete all new lines and line feeds in a {@code String}
   * @return an array of the tokens in the list
   * @see #tokenizeToStringArray
   */
  public static String[] delimitedListToStringArray(
      @Nullable String str, @Nullable String delimiter, @Nullable String charsToDelete) {

    if (str == null) {
      return new String[0];
    }
    if (delimiter == null) {
      return new String[] {str};
    }

    List<String> result = new ArrayList<>();
    if ("".equals(delimiter)) {
      for (int i = 0; i < str.length(); i++) {
        result.add(deleteAny(str.substring(i, i + 1), charsToDelete));
      }
    } else {
      int pos = 0;
      int delPos;
      while ((delPos = str.indexOf(delimiter, pos)) != -1) {
        result.add(deleteAny(str.substring(pos, delPos), charsToDelete));
        pos = delPos + delimiter.length();
      }
      if (str.length() > 0 && pos <= str.length()) {
        // Add rest of String, but not in case of empty input.
        result.add(deleteAny(str.substring(pos), charsToDelete));
      }
    }
    return toStringArray(result);
  }

  /**
   * Copy the given {@code Collection} into a {@code String} array.
   *
   * <p>The {@code Collection} must contain {@code String} elements only.
   *
   * @param collection the {@code Collection} to copy
   * @return the {@code String} array
   */
  public static String[] toStringArray(Collection<String> collection) {
    return collection.toArray(new String[0]);
  }

  /**
   * Delete any character in a given {@code String}.
   *
   * @param inString the original {@code String}
   * @param charsToDelete a set of characters to delete. E.g. "az\n" will delete 'a's, 'z's and new
   *     lines.
   * @return the resulting {@code String}
   */
  public static String deleteAny(String inString, @Nullable String charsToDelete) {
    if (!hasLength(inString) || !hasLength(charsToDelete)) {
      return inString;
    }

    StringBuilder sb = new StringBuilder(inString.length());
    for (int i = 0; i < inString.length(); i++) {
      char c = inString.charAt(i);
      if (charsToDelete.indexOf(c) == -1) {
        sb.append(c);
      }
    }
    return sb.toString();
  }

  /**
   * Convert a {@code Collection} into a delimited {@code String} (e.g. CSV).
   *
   * <p>Useful for {@code toString()} implementations.
   *
   * @param coll the {@code Collection} to convert (potentially {@code null} or empty)
   * @param delim the delimiter to use (typically a ",")
   * @return the delimited {@code String}
   */
  public static String collectionToDelimitedString(@Nullable Collection<?> coll, String delim) {
    return collectionToDelimitedString(coll, delim, "", "");
  }

  /**
   * Convert a {@code Collection} into a delimited {@code String} (e.g., CSV).
   *
   * <p>Useful for {@code toString()} implementations.
   *
   * @param coll the {@code Collection} to convert (potentially {@code null} or empty)
   * @return the delimited {@code String}
   */
  public static String collectionToCommaDelimitedString(@Nullable Collection<?> coll) {
    return collectionToDelimitedString(coll, ",");
  }

  /**
   * Convert a {@link Collection} to a delimited {@code String} (e.g. CSV).
   *
   * <p>Useful for {@code toString()} implementations.
   *
   * @param coll the {@code Collection} to convert (potentially {@code null} or empty)
   * @param delim the delimiter to use (typically a ",")
   * @param prefix the {@code String} to start each element with
   * @param suffix the {@code String} to end each element with
   * @return the delimited {@code String}
   */
  public static String collectionToDelimitedString(
      @Nullable Collection<?> coll, String delim, String prefix, String suffix) {

    if (CollectionUtils.isEmpty(coll)) {
      return "";
    }

    StringBuilder sb = new StringBuilder();
    Iterator<?> it = coll.iterator();
    while (it.hasNext()) {
      sb.append(prefix).append(it.next()).append(suffix);
      if (it.hasNext()) {
        sb.append(delim);
      }
    }
    return sb.toString();
  }
  /**
   * Replace all occurrences of a substring within a string with another string.
   *
   * @param inString {@code String} to examine
   * @param oldPattern {@code String} to replace
   * @param newPattern {@code String} to insert
   * @return a {@code String} with the replacements
   */
  public static String replace(String inString, String oldPattern, @Nullable String newPattern) {
    if (!hasLength(inString) || !hasLength(oldPattern) || newPattern == null) {
      return inString;
    }
    int index = inString.indexOf(oldPattern);
    if (index == -1) {
      // no occurrence -> can return input as-is
      return inString;
    }

    int capacity = inString.length();
    if (newPattern.length() > oldPattern.length()) {
      capacity += 16;
    }
    StringBuilder sb = new StringBuilder(capacity);

    int pos = 0; // our position in the old string
    int patLen = oldPattern.length();
    while (index >= 0) {
      sb.append(inString.substring(pos, index));
      sb.append(newPattern);
      pos = index + patLen;
      index = inString.indexOf(oldPattern, pos);
    }

    // append any characters to the right of a match
    sb.append(inString.substring(pos));
    return sb.toString();
  }

  /**
   * Apply the given relative path to the given Java resource path, assuming standard Java folder
   * separation (i.e. "/" separators).
   *
   * @param path the path to start from (usually a full file path)
   * @param relativePath the relative path to apply (relative to the full file path above)
   * @return the full file path that results from applying the relative path
   */
  public static String applyRelativePath(String path, String relativePath) {
    int separatorIndex = path.lastIndexOf(FOLDER_SEPARATOR);
    if (separatorIndex != -1) {
      String newPath = path.substring(0, separatorIndex);
      if (!relativePath.startsWith(FOLDER_SEPARATOR)) {
        newPath += FOLDER_SEPARATOR;
      }
      return newPath + relativePath;
    } else {
      return relativePath;
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
