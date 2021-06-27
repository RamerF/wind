package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对一映射.可选添加关联列{@link #shouldJoinColumn()},shouldJoinColumn必须有且仅有一个为true.
 *
 * <p><b>新增列定义和关联属性保持一致,所以使用{@link TableColumn}指定列定义将不会生效</b>.
 *
 * <p>多的一方添加列field关联目标对象referenceField并且列定义一致.<br>
 * 如果没有指定field,默认新增列[下划线分割(类型名)_id],如果没有指定referenceField,默认关联id
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToOne {

  /** 关联对象字段名.默认关联主键 */
  String targetField() default "";

  /** 当前对象的字段,使用该字段关联对方{@link #targetField()},默认:下划线[类型{@link #targetField()}]. */
  String joinField() default "";
}
