package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.support.EntityInfo;
import lombok.Getter;
import lombok.Setter;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
@Getter
@Setter
public abstract class AbstractQueryEntity<T extends AbstractEntityPoJo> implements QueryEntity<T> {
  private QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();
  private EntityInfo entityInfo;
}
