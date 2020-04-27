package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;

/**
 * @author Tang Xiaofeng
 * @since 20-1-6
 */
public interface Expression<T extends AbstractEntity> extends Condition<T>, FuncConditions<T> {}
