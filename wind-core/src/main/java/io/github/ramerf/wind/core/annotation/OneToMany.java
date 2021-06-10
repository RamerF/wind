package io.github.ramerf.wind.core.annotation;

import io.github.ramerf.wind.core.util.EntityUtils;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.lang.reflect.Field;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 指定一对多映射.
 *
 * <ul>
 *   <li>如果指定了joinName,则多的一方表中添加列joinName.
 *   <li>否则,在多的一方查找属性mappedBy(默认为当前类名的下划线形式),再获取属性的列名{@link EntityUtils#fieldToColumn(Field)}
 * </ul>
 *
 * <pre>
 *
 *    Example 1: 双向一对多
 *
 *    // In Customer class:
 *
 *    &#064;OneToMany(mappedBy="customer") // mappedBy默认为customer
 *    private List&#060;Order&#062; orders;
 *
 *    // In Order class:
 *
 *    &#064;ManyToOne
 *    &#064;JoinColumn(name="customer_id") // 默认值为customer_id
 *    private Customer customer;
 *
 *
 *    Example 2: 单向一对多
 *
 *    // In Customer class:
 *
 *    &#064;OneToMany
 *    &#064;JoinColumn(name="customer_id") // 在order表中添加列
 *    private List&#060;Order&#062; orders;
 * </pre>
 *
 * @since 2020.10.28
 * @author ramer
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OneToMany {
  /** 维护关系的属性,为多的一方属性.默认当前类的驼峰形式,如:customer */
  String mappedBy() default "";

  /** 多的一方添加的列名,默认为[当前类的下划线形式_id],如:customer_id. */
  String joinColumn() default "";
}
