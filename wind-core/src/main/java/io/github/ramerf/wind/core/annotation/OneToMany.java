package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对多映射.
 *
 * <p>多的一方添加列field关联目标对象referenceField并且列定义相同.<br>
 * 如果没有指定field,默认新增列[下划线分割(类型名)_id],如果没有指定referenceField,默认关联id
 *
 * @since 2020.10.28
 * @author Tang Xiaofeng
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToMany {
  /** 当前对象属性名.默认id */
  String field() default "id";

  /** 关联对象属性名.默认[当前类Id],如:fooId */
  String referenceField() default "";
}
