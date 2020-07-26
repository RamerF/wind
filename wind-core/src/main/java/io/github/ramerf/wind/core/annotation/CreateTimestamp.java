package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;

/**
 * 创建时间,当实体创建时,属性值被设置为当前VM时间.
 *
 * @author ramer
 * @since 21 /07/2020
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface CreateTimestamp {}
