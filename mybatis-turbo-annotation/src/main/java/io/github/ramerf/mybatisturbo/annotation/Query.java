package io.github.ramerf.mybatisturbo.annotation;

import java.lang.annotation.*;

/**
 * 指定数据库表映射.
 *
 * @author Tang Xiaofeng
 * @since 2019/12/29
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface Query {
  /** 数据库字段名. */
  String alia();
}
