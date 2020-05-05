package io.github.ramerf.wind.core.entity.constant;

import java.time.ZoneOffset;

/**
 * 系统常量定义.
 *
 * @author Tang Xiaofeng
 * @since 2019/11/12
 */
@SuppressWarnings({"unused"})
public class Constant {

  /** 默认list字符串分隔符 */
  public static final String DEFAULT_STRING_SPLIT = ",";
  /** 逗号. */
  public static final String SEMICOLON = ",";

  /** 默认list字符串以空格为分割符 */
  public static final String DEFAULT_SPLIT_SPACE = " ";

  public static final String DEFAULT_STRING_SHORT_LINE = "-";

  public static final String DEFAULT_STRING_BLANK = "";

  public static final String DEFAULT_STRING_POINT = ".";

  public static final String DEFAULT_STRING_SEMICOLON = ";";

  public static final String DEFAULT_STRING_COLON = ":";

  public static final String DEFAULT_CHARSET_ENCODE = "UTF-8";
  /** 分页每页默认条数 */
  public static final int DEFAULT_PAGE_SIZE = 10;
  /** 默认时区 */
  public static final ZoneOffset DEFAULT_ZONE = ZoneOffset.of("+8");
}
