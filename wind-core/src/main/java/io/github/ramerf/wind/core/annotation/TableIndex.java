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
public @interface TableIndex {
  /**
   * 索引前缀.默认为 {@code idx_table_name}
   *
   * <p>{@code name}不为空时该值无效
   */
  String prefix() default "";

  Index[] value();

  @interface Index {
    /** 名称.默认为 {@code prefix + indexField},下划线分割 */
    String name() default "";
    /** 是否唯一索引. */
    boolean unique() default false;
    /** 索引字段. */
    IndexField[] indexFields();
    /** 备注. */
    String comment() default "";
  }

  @interface IndexField {

    /** java属性名. */
    String field();

    int length() default -1;

    /** 索引规则. */
    Direction direction() default Direction.ASC;
  }
}
