package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对一映射.可选添加关联列
 *
 * <p>多的一方添加列field关联目标对象referenceField并且列定义相同.<br>
 * 如果没有指定field,默认新增列[下划线分割(类型名)_id],如果没有指定referenceField,默认关联id
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToOne {
  /** 当前对象属性名.默认[属性类型Id],如:fooId */
  String field() default "";

  /** 关联对象属性名.默认关联id */
  String referenceField() default "id";

  /** 是否新增列. */
  boolean joinColumn() default true;

  /** 当{@code joinColumn == true}时添加的列名,默认:下划线[类型_{@link #referenceField()}]. */
  String joinColumnName() default "";
}
