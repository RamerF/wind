package io.github.ramerf.wind.core.entity.pojo;

import io.github.ramerf.wind.core.condition.LambdaCondition;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.service.InterService.Fields;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.function.Consumer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;

/**
 * PoJo实体.
 *
 * @since 2019 /12/6
 * @author Tang Xiaofeng
 */
@Slf4j
public class AbstractEntityPoJo<T extends AbstractEntityPoJo<T, ID>, ID extends Serializable>
    implements AbstractEntity {
  /**
   * 创建记录.
   *
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  public final AbstractEntityPoJo<T, ID> create() throws DataAccessException {
    return genericService().create(instance());
  }

  /**
   * 创建记录.
   *
   * @param fieldsConsumer the fields consumer
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  public final AbstractEntityPoJo<T, ID> create(final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    return genericService().create(instance(), fieldsConsumer);
  }

  /**
   * 更新.
   *
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update() throws DataAccessException {
    return genericService().update(instance());
  }

  /**
   * 更新.
   *
   * @param fieldsConsumer the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Consumer<Fields<T>> fieldsConsumer) throws DataAccessException {
    return genericService().update(instance(), fieldsConsumer, null);
  }

  /**
   * 更新.
   *
   * @param conditionConsumer the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int updateByCondition(final Consumer<LambdaCondition<T>> conditionConsumer)
      throws DataAccessException {
    return genericService().update(instance(), null, conditionConsumer);
  }

  /**
   * 删除记录.
   *
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   * @see CommonException
   */
  @SuppressWarnings("unchecked")
  public int delete() throws DataAccessException {
    final Field idField = EntityHelper.getEntityIdField(this.getClass());
    return genericService().delete((ID) BeanUtils.getValue(this, idField, null));
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.示例:<br>
   *     {@code condition -> condition.eq(AbstractEntityPoJo::setId, 1L)}
   * @return 删除记录数 long
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  public int delete(Consumer<LambdaCondition<T>> consumer) throws DataAccessException {
    return genericService().delete(consumer);
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  @SuppressWarnings("unchecked")
  private T instance() {
    return (T) this;
  }

  @SuppressWarnings("unchecked")
  private GenericService<T, ID> genericService() {
    return GenericService.with(
        (Class<T>) instance().getClass(),
        (Class<ID>) EntityHelper.getEntityIdField(instance().getClass()).getType());
  }
}
