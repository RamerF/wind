package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;

/**
 * 通用业务方法.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019/11/13
 */
public interface BaseService<T extends AbstractEntityPoJo>
    extends QueryService<T>, UpdateService<T> {
  /**
   * 创建记录,返回创建成功的对象.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return the T
   * @throws RuntimeException 创建失败时,抛异常
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  default T createAndGet(@Nonnull final T t) throws RuntimeException {
    create(t, null);
    return getById(t.getId());
  }

  /**
   * 创建记录,返回创建成功的对象.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param fieldsConsumer the fields consumer
   * @return the T
   * @throws RuntimeException 创建失败时,抛异常
   */
  default T createAndGet(@Nonnull final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws RuntimeException {
    create(t, fieldsConsumer);
    return getById(t.getId());
  }

  /**
   * 更新不为null的字段,返回更新成功的对象.
   *
   * @param t the t
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateAndGet(final T t) throws RuntimeException {
    update(t, null);
    return getById(t.getId());
  }

  /**
   * 更新.返回更新成功的对象.
   *
   * @param t the t
   * @param fieldsConsumer the fields consumer
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateAndGet(final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws RuntimeException {
    update(t, fieldsConsumer);
    return getById(t.getId());
  }

  /**
   * 更新.返回更新成功的对象.
   *
   * @param t the t
   * @param conditionConsumer the fields consumer
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateByConditionAndGet(final T t, final Consumer<Condition<T>> conditionConsumer)
      throws RuntimeException {
    updateByCondition(t, conditionConsumer);
    return getById(t.getId());
  }

  /**
   * 更新.返回更新成功的对象.
   *
   * @param t the t
   * @param conditionConsumer the fields consumer
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateAndGet(
      final T t,
      final Consumer<Fields<T>> fieldsConsumer,
      final Consumer<Condition<T>> conditionConsumer)
      throws RuntimeException {
    update(t, fieldsConsumer, conditionConsumer);
    return getById(t.getId());
  }
}
