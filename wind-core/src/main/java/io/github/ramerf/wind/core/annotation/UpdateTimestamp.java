package io.github.ramerf.wind.core.annotation;

import java.lang.annotation.*;
import java.time.*;
import java.util.Date;

/**
 * 更新时间,当实体更新时,属性值被设置为当前JVM时间.
 *
 * <p>仅支持{@link LocalDate},{@link LocalTime},{@link LocalDateTime},{@link Date},{@link Long
 *
 * @since 2020.11.23
 * @author ramer
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface UpdateTimestamp {}
