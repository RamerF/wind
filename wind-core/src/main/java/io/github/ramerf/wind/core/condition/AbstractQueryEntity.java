package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.support.EntityInfo;
import lombok.*;

/**
 * @author ramer
 * @since 2019/12/28
 */
public abstract class AbstractQueryEntity<T> implements QueryEntity<T> {
  @Getter
  @Setter(AccessLevel.PROTECTED)
  private QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();

  @Getter(AccessLevel.PROTECTED)
  @Setter(AccessLevel.PROTECTED)
  private EntityInfo entityInfo;
}
