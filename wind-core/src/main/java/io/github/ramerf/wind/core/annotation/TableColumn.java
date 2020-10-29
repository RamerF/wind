package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;
import javax.persistence.Column;

/**
 * 列信息.
 *
 * @author ramer
 * @since 21/07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface TableColumn {
  /** 列名默认为该属性的下划线格式,如果是关联属性,默认为[关联类名_属性名]的下划线格式. */
  String name() default "";

  /**
   * (可选)默认不要拉取该字段. 设置为true时,只有手动指定要查询该字段时才会拉取,如: <br>
   * <code>
   *   service.getOne(query -> query.col(Foo::getName), Foo.class);
   * </code>
   */
  boolean dontFetch() default false;

  /**
   * 注释.该值优先级最高.
   *
   * <p>对于MYSQL等支持在列定义后直接跟comment的数据库,{@link Column#columnDefinition()}中的comment会被覆盖.
   */
  String comment() default "";

  /**
   * 默认值.<b>如果是空串,需要设置{@link #defaultBlankValue()}为true</b>
   *
   * <p>{@link Column#columnDefinition()}中的default会被覆盖.
   */
  String defaultValue() default "";

  /** 指定默认值为空串. */
  boolean defaultBlankValue() default false;
}
