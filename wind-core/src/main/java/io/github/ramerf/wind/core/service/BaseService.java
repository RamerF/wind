package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IFunction;
import javax.annotation.Nonnull;
import org.springframework.dao.DataAccessException;

/**
 * 通用业务方法.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2019 /11/13
 */
@SuppressWarnings({"unused", "unchecked"})
public interface BaseService<T extends AbstractEntityPoJo>
    extends QueryService<T>, UpdateService<T> {
  /**
   * 创建记录,返回创建成功的对象.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param includeNullProps 即使值为null也保存的属性
   * @return the T
   * @throws RuntimeException 创建失败时,抛异常
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  default T createAndGet(@Nonnull final T t, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    create(t, includeNullProps);
    return getById(t.getId());
  }

  /**
   * 更新不为null的字段.返回更新成功的对象.
   *
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T updateAndGet(final T t, final IFunction<T, ?>... includeNullProps)
      throws RuntimeException {
    update(t, includeNullProps);
    return getById(t.getId());
  }
}
