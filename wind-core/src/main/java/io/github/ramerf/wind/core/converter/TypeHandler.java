package io.github.ramerf.wind.core.converter;

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
public @interface TypeHandler {
  @SuppressWarnings("rawtypes")
  Class<? extends TypeConverter> value();
}
