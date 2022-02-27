package io.github.ramerf.wind.core.util;

import javax.annotation.Nullable;

/**
 * @since 2022.01.13
 * @author ramer
 */
public final class Asserts {
  public static void notNull(@Nullable final Object object, final String message) {
    if (object == null) {
      throw new IllegalArgumentException(message);
    }
  }

  /** 包含非空白字符. */
  public static void hasText(@Nullable String text, final String message) {
    if (!StringUtils.hasText(text)) {
      throw new IllegalArgumentException(message);
    }
  }

  public static void isTrue(@Nullable Boolean condition, final String message) {
    if (condition != null && condition) {
      throw new IllegalArgumentException(message);
    }
  }
}
