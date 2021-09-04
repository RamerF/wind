package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.io.Serializable;
import java.lang.reflect.Field;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;

/**
 * 通用业务方法.
 *
 * @param <T> 实体
 * @param <ID> 主键类型
 * @author ramer
 * @since 2019/11/13
 */
public interface BaseService<T, ID extends Serializable>
    extends QueryService<T, ID>, UpdateService<T, ID> {
  /** 创建记录,返回创建成功的对象. */
  default <E> E createAndGet(@Nonnull final E e) throws DataAccessException {
    return createAndGet(e, null);
  }

  /** 创建记录,返回创建成功的对象. */
  default <E> E createAndGet(@Nonnull final E e, final Fields<E> fields)
      throws DataAccessException {
    create(e, fields);
    @SuppressWarnings("unchecked")
    final Class<E> clazz = (Class<E>) e.getClass();
    final Field idField = EntityHelper.getEntityIdField(clazz);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(e, idField, null);
    return getOne(Cnds.of(clazz).eq(idField, id));
  }

  /** 更新不为null的字段,返回更新成功的对象. */
  default <E> E updateAndGet(final E e) throws DataAccessException {
    return updateAndGet(e, null);
  }

  /** 更新.返回更新成功的对象. */
  default <E> E updateAndGet(final E e, final Fields<E> fields) throws DataAccessException {
    update(e, fields, null);
    @SuppressWarnings("unchecked")
    final Class<E> clazz = (Class<E>) e.getClass();
    final Field idField = EntityHelper.getEntityIdField(clazz);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(e, idField, null);
    return getOne(Cnds.of(clazz).eq(idField, id));
  }

  /** 更新.返回更新成功的对象 */
  default T updateAndGet(final T t, final Cnd<T, ?, ?> cnd) throws DataAccessException {
    update(t, null, cnd);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
  }

  /** 更新.返回更新成功的对象. */
  default T updateAndGet(final T t, final Fields<T> fields, final Cnd<T, ?, ?> cnd)
      throws DataAccessException {
    update(t, fields, cnd);
    @SuppressWarnings("unchecked")
    final ID id = (ID) BeanUtils.getValue(t, EntityHelper.getEntityIdField(t.getClass()), null);
    return getById(id);
  }
}
