package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 逻辑删除.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface LogicDelete {

  /**
   * 是否支持逻辑删除,默认启用(true),设置为false时将不支持逻辑删除功能.
   *
   * @return the boolean
   */
  boolean enable() default false;

  /**
   * 逻辑删除java字段名,最终以下划线形式对应数据库列,糟了😥,这里有问题,如果数据库列名不是下划线形式就洗白了,哈哈.
   *
   * @return the string
   */
  String fieldName();

  /**
   * 逻辑已删除值.
   *
   * @return the boolean
   */
  boolean deleted() default true;

  /**
   * 逻辑未删除值.
   *
   * @return the boolean
   */
  boolean notDelete() default false;
}
