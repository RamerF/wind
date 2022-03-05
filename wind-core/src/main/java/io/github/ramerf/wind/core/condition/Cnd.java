package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.domain.Pageable;
import javax.annotation.Nonnull;

/**
 * The interface Cnd.
 *
 * @param <POJO> pojo对象
 * @param <CND> 当前对象
 * @param <CONDITION> 持有的Condition对象,比如:LambdaCondition,StringCondition
 * @since 14 /08/2021
 * @author ramer
 */
public interface Cnd<
    POJO, CND extends Condition<POJO, CND>, CONDITION extends Condition<POJO, CONDITION>> {
  @Nonnull
  Class<POJO> getClazz();

  @Nonnull
  Pageable getPageRequest();

  @Nonnull
  CONDITION getCondition();
}
