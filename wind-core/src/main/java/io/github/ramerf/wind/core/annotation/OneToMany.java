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
 *    示例一: 双向一对多
 *
 *    // Customer 类:
 *    private long id;
 *    // 这里targetField默认为customer,可不指定
 *    &#064;OneToMany(targetField = "customer")
 *    private List&#060;Order&#062; orders;
 *
 *    // Order 类:
 *
 *    &#064;ManyToOne
 *    private Customer customer;
 *    private Long customerId;
 *
 *
 *    示例二: 单向一对多
 *
 *    // Customer 类:
 *
 *    &#064;OneToMany
 *    private List&#060;Order&#062; orders;
 *
 *    // Order 类
 *    private Long customerId;
 *
 *
 *    示例三: 单向一对多,指定关联属性
 *
 *    // Customer 类:
 *    private String uid;
 *
 *    &#064;OneToMany(targetField = "customerUid")
 *    private List&#060;Order&#062; orders;
 *
 *    // Order 类
 *    &#064;ManyToOne&#060;(targetField = "uid")&#062;
 *    private String customerUid;
 *
 * </pre>
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToMany {
  /** 多的一方添加的属性,用来关联一的一方. */
  String targetField();
}
