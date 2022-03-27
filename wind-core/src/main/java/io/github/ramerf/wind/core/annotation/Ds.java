package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 支持动态切换数据源.
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface Ds {
  /** 数据源名称. */
  String value();
}
