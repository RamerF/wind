package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * The interface Condition.
 *
 * <p>TODO 如果要实现完整的连表查询,考虑在该类添加join方法
 *
 * @param <T> the type parameter
 * @since 2020.01.06
 * @author Tang Xiaofeng
 */
public interface ICondition<T extends AbstractEntityPoJo> extends Predicate<T> {

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @return the Condition
   * @see Condition#of(QueryColumn)
   */
  default ICondition<T> condition() {
    return condition(false);
  }

  /**
   * 创建一个空的条件,包含表信息.
   *
   * @param genAlia 是否生成新的表别名,用于子查询时传true
   * @return the Condition
   * @see Condition#of(QueryColumn)
   */
  ICondition<T> condition(final boolean genAlia);

  /**
   * 获取占位符值.<br>
   *
   * @param startIndex 填充参数的起始索引,null时从0开始
   * @return 占位符对应的值 values
   */
  List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex);

  /**
   * 获取所有原始值.
   *
   * @return the value string
   */
  List<Object> getOriginValues();

  /**
   * 是否为空,true:不包含任何条件.
   *
   * @return the boolean
   */
  boolean isEmpty();
}
