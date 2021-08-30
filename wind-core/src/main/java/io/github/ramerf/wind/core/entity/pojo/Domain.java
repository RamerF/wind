package io.github.ramerf.wind.core.entity.pojo;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.service.GenericLambdaService;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.Serializable;
import java.lang.reflect.Field;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;

/**
 * 实体域.
 *
 * @since 2021.02.06
 * @author ramer
 */
@Slf4j
public class Domain<T, ID extends Serializable> {
  /**
   * 创建记录.
   *
   * @throws DataAccessException 如果执行失败
   */
  public final int create() throws DataAccessException {
    return genericService().create(instance());
  }

  /**
   * 创建记录.
   *
   * @param fields 保存指定列
   * @return 包含主键
   * @throws DataAccessException 如果执行失败
   */
  public final int create(final Fields<T> fields) throws DataAccessException {
    return genericService().create(instance(), fields);
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
   * 更新指定字段.
   *
   * @param fields the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Fields<T> fields) throws DataAccessException {
    return genericService().update(instance(), fields, null);
  }

  /**
   * 更新.
   *
   * @param cnd the fields consumer
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Cnd<T, ?, ?> cnd) throws DataAccessException {
    return genericService().update(instance(), null, cnd);
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
   * @param condition 示例: {@code LambdaCondition.of(Foo.class).eq(Foo::setId, 1L)}
   * @return 删除记录数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  public int delete(Condition<T, ?> condition) throws DataAccessException {
    return genericService().delete(condition);
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
  private GenericLambdaService<T, ID> genericService() {
    return GenericLambdaService.with(
        (Class<T>) instance().getClass(),
        (Class<ID>) EntityHelper.getEntityIdField(instance().getClass()).getType());
  }
}
