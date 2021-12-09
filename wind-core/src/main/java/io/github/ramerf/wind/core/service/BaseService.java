package io.github.ramerf.wind.core.service;

import java.io.Serializable;

/**
 * 通用业务方法.
 *
 * @param <T> 实体
 * @param <ID> 主键类型
 * @author ramer
 * @since 2019/11/13
 */
public interface BaseService<T, ID extends Serializable>
    extends QueryService<T, ID>, UpdateService<T, ID> {}
