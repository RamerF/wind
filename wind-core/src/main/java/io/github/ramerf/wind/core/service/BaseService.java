package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;

/**
 * 通用业务方法.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /11/13
 */
public interface BaseService<T extends AbstractEntityPoJo>
    extends QueryService<T>, UpdateService<T> {}
