package io.github.ramerf.wind.core.handler.typehandler;

import java.lang.annotation.*;

/**
 * 表示注解的类型处理器不会注册,需要手动在字段上指定才能使用.
 *
 * @author Tang Xiaofeng
 * @since 2020.12.27
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@interface Skip {}
