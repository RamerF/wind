package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对一映射.
 *
 * <pre>
 *
 *    示例一: 双向一对一
 *
 *    // Customer 类:
 *    private long id;
 *    &#064;OnetoOne
 *    private Department department;
 *    private Long departmentId;
 *
 *    // Department class:
 *    &#064;OneToOne
 *    private Customer customer;
 *    private Long customerId;
 *
 *    示例二: 单向一对一
 *
 *    // Customer 类:
 *    private long id;
 *    &#064;OnetoOne
 *    private Department department;
 *    private Long departmentId;
 *
 *    示例三: 单向一对一,关联指定字段
 *
 *    // Customer 类:
 *    private long id;
 *    // targetField默认id,joinField默认为fieldType + targetField
 *    &#064;OnetoOne(targetField = "no", joinField = "departmentNo")
 *    private Department department;
 *    private String departmentNo;
 *
 *    // Department 类:
 *    private String no;
 * </pre>
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToOne {

  /** 关联对象字段名.默认关联主键 */
  String targetField() default "";

  /** 当前对象添加的字段,使用该字段关联对方{@link #targetField()},默认:下划线[类型{@link #targetField()}]. */
  String joinField() default "";
}
