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

  /** 是否支持逻辑删除,默认禁用(false),设置为true时支持逻辑删除功能. */
  boolean enable();

  /**
   * 逻辑删除java字段名,最终以下划线形式对应数据库列,糟了😥,这里有问题,如果数据库列名不是下划线形式就洗白了,哈哈.
   *
   * <p>在一个风雨交加的白天,我悄悄修复了这个bug
   */
  String fieldName() default DEFAULT_FIELD_NAME;

  /** 逻辑已删除值. */
  boolean deleted() default true;

  /** 逻辑未删除值. */
  boolean notDelete() default false;

  String DEFAULT_FIELD_NAME = "XX_FIELD_NAME_XX";
}
