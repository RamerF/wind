package io.github.ramerf.wind.core.entity.pojo;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.service.GenericService;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.Serializable;
import java.lang.reflect.Field;
import lombok.extern.slf4j.Slf4j;

/**
 * 实体域.
 *
 * @param <T> 实体对象
 * @param <ID> 主键类型
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
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Fields<T> fields) throws DataAccessException {
    return genericService().update(instance(), fields, null);
  }

  /**
   * 更新.
   *
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Cnd<T, ?, ?> cnd) throws DataAccessException {
    return genericService().update(instance(), null, cnd);
  }

  /**
   * 更新.
   *
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  public final int update(final Cnd<T, ?, ?> cnd, final Fields<T> fields)
      throws DataAccessException {
    return genericService().update(instance(), fields, cnd);
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
   * @param condition the condition.
   *     <p>示例:
   *     <li>{@code Cnds.of(Foo.class).eq(Foo::setId, id)}
   *     <li>{@code LambdaCondition.of(Foo.class).eq(Foo::setId, 1L)}
   * @return 删除记录数 long
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   */
  public int delete(Condition<T, ?> condition) throws DataAccessException {
    return genericService().delete(condition);
  }

  private T instance() {
    //noinspection unchecked
    return (T) this;
  }

  private GenericService<T, ID> genericService() {
    //noinspection unchecked
    return GenericService.with(
        (Class<T>) instance().getClass(),
        (Class<ID>) EntityHelper.getEntityIdField(instance().getClass()).getType());
  }
}
