package io.github.ramerf.wind.core.annotation;

import io.github.ramerf.wind.core.domain.Sort.Direction;
import java.lang.annotation.*;

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
    /** 是否唯一索引. */
    boolean unique() default false;

    IndexField[] indexFields();
    /** 备注. */
    String comment() default "";
  }

  @interface IndexField {

    /** java字段名. */
    String field();

    int length() default -1;

    /** 排序. */
    Direction direction() default Direction.ASC;
  }
}
