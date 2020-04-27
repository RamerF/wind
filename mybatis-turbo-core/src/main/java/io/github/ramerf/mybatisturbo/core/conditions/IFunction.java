package io.github.ramerf.mybatisturbo.core.conditions;

import java.io.Serializable;
import java.util.function.Function;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/26
 */
@FunctionalInterface
public interface IFunction<T, R> extends Function<T, R>, Serializable {}
