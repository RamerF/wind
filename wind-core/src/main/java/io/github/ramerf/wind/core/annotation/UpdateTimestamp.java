package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 更新时间,当实体更新时,属性值被设置为当前VM时间.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface UpdateTimestamp {}
