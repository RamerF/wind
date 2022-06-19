package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.annotation.*;

/**
 * 表示注解的类需要跳过自动扫描.
 *
 * @author ramer
 * @since 2020.12.27
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@interface IgnoreScan {}
