package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.LambdaCondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.Serializable;
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
public interface BaseService<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
    extends QueryService<T, ID>, UpdateService<T, ID> {
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
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
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
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
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
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
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
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
  }

  /**
   * 更新.返回更新成功的对象.
   *
   * @param t the t
   * @param conditionConsumer the fields consumer
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateByConditionAndGet(final T t, final Consumer<LambdaCondition<T>> conditionConsumer)
      throws RuntimeException {
    updateByCondition(t, conditionConsumer);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
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
      final Consumer<LambdaCondition<T>> conditionConsumer)
      throws RuntimeException {
    update(t, fieldsConsumer, conditionConsumer);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
  }
}
