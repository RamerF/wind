package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface ConfigurationProperties {
  /** Alia for {@link #prefix()} */
  String value() default "";

  /** Alia for {@link #value()} */
  String prefix() default "";

  /** 忽略配置中无效/错误的值 */
  boolean ignoreInvalidValues() default false;

  /** 忽略配置中的未知属性 */
  boolean ignoreUnknownFields() default true;
}
