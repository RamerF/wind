package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定多对一映射.需要添加列关联一的一方
 *
 * <p>多的一方添加列field关联目标对象referenceField并且列定义相同.<br>
 * 如果没有指定field,默认新增列[下划线分割(类型名)_id],如果没有指定referenceField,默认关联id
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface ManyToOne {
  /** 当前对象属性名.默认[属性类型Id],如:fooId */
  String field() default "";

  /** 关联对象属性名.默认关联id */
  String referenceField() default "id";

  /** 添加列名,默认:下划线[类型_{@link #referenceField()}]. */
  String joinColumnName() default "";
}
