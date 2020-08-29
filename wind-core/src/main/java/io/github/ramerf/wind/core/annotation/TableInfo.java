package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 表信息.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface TableInfo {

  /**
   * 表名.
   *
   * @return the string
   */
  String name();

  /** 备注. */
  String comment() default "";

  LogicDelete logicDelete() default @LogicDelete;
}
