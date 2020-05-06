package io.github.ramerf.wind.core.function;

import java.util.function.Function;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@FunctionalInterface
public interface IFunction<T, U> extends Function<T, U>, BeanFunction {}
