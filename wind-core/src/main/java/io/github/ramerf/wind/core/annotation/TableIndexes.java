package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;
import org.springframework.data.domain.Sort.Direction;

/**
 * 表索引信息.
 *
 * @author ramer
 * @since 21/07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface TableIndexes {
  Index[] value();

  @interface Index {

    /** 名称. */
    String name();

    /** java字段名. */
    String[] fields();

    /** 是否唯一索引. */
    boolean unique() default false;

    /** 备注. */
    String comment() default "";

    /** 排序. */
    Direction direction() default Direction.ASC;
  }
}
