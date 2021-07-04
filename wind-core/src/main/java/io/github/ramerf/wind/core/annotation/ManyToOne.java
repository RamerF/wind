package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import javax.persistence.Id;

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
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface ManyToOne {

  /** 关联对象属性名.默认关联主键{@link Id} */
  String targetField() default "";

  /** 当前对象字段,默认:驼峰[目标类型名{@link #targetField()}]. */
  String joinField() default "";
}
