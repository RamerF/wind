package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对多映射.
 *
 * <pre>
 *
 *    Example 1: 双向一对多
 *
 *    // In Customer class:
 *    private long id;
 *
 *    &#064;OneToMany(targetField = "customer") // 双向关联时,目标属性是对象
 *    private List&#060;Order&#062; orders;
 *
 *    // In Order class:
 *
 *    &#064;ManyToOne(joinField = "customerId", targetField = "id")// joinField默认为customerId
 *    private Customer customer;
 *
 *    private Long customerId; // 无论单向还是双向都必须添加关联属性
 *
 *
 *    Example 2: 单向一对多
 *
 *    // In Customer class:
 *
 *    &#064;OneToMany(targetField = "customerId")  // 单向时,目标属性是基本属性
 *    private List&#060;Order&#062; orders;
 * </pre>
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToMany {
  /** 多关联一的属性.双向关联时字段类型为对象,单向关联时为普通字段. */
  String targetField();
}
