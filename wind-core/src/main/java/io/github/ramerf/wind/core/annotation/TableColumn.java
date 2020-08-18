package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 字段信息.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface TableColumn {

  /**
   * (可选)默认不要拉取该字段. 设置为true时,只有手动指定要查询该字段时才会拉取,如: <br>
   * <code>
   *   service.getOne(query -> query.col(Foo::getName), Foo.class);
   * </code>
   */
  boolean dontFetch() default false;

  /** 注释. */
  String comment() default "";
}
