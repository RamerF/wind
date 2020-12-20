package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 列信息.
 *
 * @author ramer
 * @since 21/07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface TableColumn {

  /** 指定字符类型的列的长度. */
  int length() default 255;

  /** 指定数字类型的列的精度,整数位数. */
  int precision() default 0;

  /** 指定数字类型的列的精度,小数位数. */
  int scale() default 0;

  /** 是否添加唯一约束.默认否 */
  boolean unique() default false;

  /** 是否可为空,默认可为空. */
  boolean nullable() default true;

  /**
   * 列定义.如:smallint(1) default 1
   *
   * @see #defaultValue()
   * @see #defaultBlankValue()
   * @see #comment()
   */
  String columnDefinition() default "";

  /** 列名默认为该属性的下划线格式,如果是关联属性,默认为[关联类名_属性名]的下划线格式. */
  String name() default "";

  /**
   * 注释.该值优先级最高.
   *
   * <p>对于MYSQL等支持在列定义后直接跟comment的数据库,{@link #columnDefinition()}中的comment会被覆盖.
   */
  String comment() default "";

  /**
   * 默认值.<b>如果是空串,需要设置{@link #defaultBlankValue()}为true</b>
   *
   * <p>{@link #columnDefinition()}中的default会被覆盖.
   */
  String defaultValue() default "";

  /** 指定默认值为空串. */
  boolean defaultBlankValue() default false;
}
