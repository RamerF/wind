package io.github.ramerf.wind.core.function;

import java.util.function.Function;

/**
 * @author ramer
 * @since 2019/12/26
 */
@FunctionalInterface
public interface GetterFunction<T, U> extends Function<T, U>, FieldFunction {}
