package io.github.ramerf.wind.core.annotation;

import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.support.IdGenerator.VoidIdGenerator;
import java.lang.annotation.*;

/**
 * 表信息.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface TableInfo {

  /** 表名. */
  String name() default "";

  /** 备注. */
  String comment() default "";

  /** 支持逻辑删除. */
  LogicDelete logicDelete() default @LogicDelete(enable = false);

  /** id生成器,默认使用全局配置.该值会覆盖全局配置 */
  Class<? extends IdGenerator> idGenerator() default VoidIdGenerator.class;
}
