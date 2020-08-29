package io.github.ramerf.wind.core.util;

import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;

/**
 * @author ramer
 * @since 22/08/2020
 */
@Slf4j
public class NumberUtils {
  public static void doIfGreaterThanZero(final Integer num, final Consumer<Integer> consumer) {
    doIfGreaterThan(num, 0, consumer);
  }

  public static void doIfGreaterThanZero(final int num, final Consumer<Integer> consumer) {
    doIfGreaterThan(num, 0, consumer);
  }

  public static void doIfGreaterThan(
      final int num, final int min, final Consumer<Integer> consumer) {
    if (num > min) {
      consumer.accept(num);
    }
  }

  public static void doIfGreaterThan(
      final Integer num, final int min, final Consumer<Integer> consumer) {
    if (num != null && num > min) {
      consumer.accept(num);
    }
  }

  public static void main(String[] args) {
    doIfGreaterThan(-1, 0, o -> log.info("main:[{}]", o));
    doIfGreaterThanZero(1, o -> log.info("main:[{}]", o));

    doIfGreaterThan(new Integer(-2), 0, o -> log.info("main:[{}]", o));
    doIfGreaterThanZero(new Integer(2), o -> log.info("main:[{}]", o));
    doIfGreaterThan(null, 0, o -> log.info("main:[{}]", o));
  }
}
