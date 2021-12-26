package io.github.ramerf.wind.core.ioc;

import java.util.List;
import lombok.extern.slf4j.Slf4j;

/**
 * 未返回期望的结果数量.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class NonUniqueBeanException extends RuntimeException {
  private String name;
  private Class<?> clazz;
  private final int count;
  private final List<?> list;

  public NonUniqueBeanException(final String name, List<?> list) {
    this.name = name;
    this.count = list.size();
    this.list = list;
  }

  public NonUniqueBeanException(final Class<?> clazz, List<?> list) {
    this.clazz = clazz;
    this.count = list.size();
    this.list = list;
  }

  @Override
  public String toString() {
    return String.format(
        "Expect unique bean %s,but found: %s", (name != null ? name : clazz), list);
  }
}
