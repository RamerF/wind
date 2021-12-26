package io.github.ramerf.wind.core.ioc;

import lombok.extern.slf4j.Slf4j;

/**
 * 未返回期望的结果数量.
 *
 * @since 2021.01.31
 * @author ramer
 */
@Slf4j
public class NoSuchBeanException extends RuntimeException {
  private Class<?> clazz;
  private String name;

  public NoSuchBeanException(final Class<?> clazz) {
    this.clazz = clazz;
  }

  public NoSuchBeanException(final String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return "no such bean found: " + (name != null ? name : clazz);
  }
}
