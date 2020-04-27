package io.github.ramerf.mybatisturbo.core.conditions;

import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.response.ResultCode;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;

/**
 * @author Tang Xiaofeng
 * @since 2020/1/5
 */
public class FuncCondition<T extends AbstractEntity> extends AbstractQueryEntity<T>
    implements FuncConditions<T> {

  @Override
  public String getString() {
    return getCondition();
  }

  @Override
  public String getCondition() {
    throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
  }
}
