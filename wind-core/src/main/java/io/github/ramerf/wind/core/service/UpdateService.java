package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.util.List;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

/**
 * The interface Update service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(UpdateService.class);

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return the {@link AbstractEntityPoJo}
   * @throws RuntimeException 创建失败时,抛异常
   * @see DataAccessException
   */
  default long create(T t) throws RuntimeException {
    textFilter(t, t);
    final int row = getUpdate().create(t);
    if (row != 1) {
      throw CommonException.of(ResultCode.ERROR);
    }
    return t.getId();
  }

  /**
   * Create batch list.
   *
   * @param ts the ts
   * @return the list
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default int createBatch(List<T> ts) throws RuntimeException {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    return CollectionUtils.isEmpty(ts) ? 0 : getUpdate().createBatch(ts);
  }

  /**
   * 条件更新,<b>不更新值为<code>null</code>的列</b>.
   *
   * @param t the t
   * @param consumer the consumer
   * @return 更新记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long updateBatch(@Nonnull final T t, @Nonnull final Consumer<ICondition<T>> consumer)
      throws RuntimeException {
    //    return getUpdate(true).where(consumer).update(t, false);
    return 0;
  }

  /**
   * 条件更新,<b>更新所有列(即使值为<code>null</code>的)</b>.
   *
   * @param t the t
   * @param consumer the consumer
   * @return 更新记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long updateBatchAll(@Nonnull final T t, @Nonnull final Consumer<ICondition<T>> consumer)
      throws RuntimeException {
    //    return getUpdate(true).where(consumer).update(t, true);
    return 0;
  }

  /**
   * 条件删除.
   *
   * @param consumer the consumer.<br>
   *     示例:
   *     <pre>
   *      condition -&gt; condition.eq(AbstractEntityPoJo::setId)
   *     </pre>
   *
   * @return 删除记录数 long
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long delete(Consumer<ICondition<T>> consumer) throws RuntimeException {
    return getUpdate().where(consumer).delete();
  }
}
