package io.github.ramerf.wind.core.generator;

import java.lang.annotation.*;

/**
 * 用于生成InterEnum子类,枚举的通用字段.
 *
 * @since 2020.11.02
 * @author Tang Xiaofeng
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.SOURCE)
public @interface CommonEnum {
  Class<?> valueType() default Integer.class;
}
