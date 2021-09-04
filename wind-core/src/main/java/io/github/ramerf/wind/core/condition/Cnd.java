package io.github.ramerf.wind.core.condition;

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
  Class<POJO> getClazz();

  Pages getPages();

  CONDITION getCondition();
}
