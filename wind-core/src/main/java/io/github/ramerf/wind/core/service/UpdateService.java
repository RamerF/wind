package io.github.ramerf.wind.core.service;

import com.baomidou.mybatisplus.core.enums.SqlMethod;
import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.condition.Update;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.util.EntityUtils;
import io.github.ramerf.wind.core.util.SnowflakeIdWorker;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

/**
 * The interface Update service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings("rawtypes")
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
  default T create(T t) throws RuntimeException {
    textFilter(t, t);
    final Date now = new Date();
    log.info("create:新增数据时间[{}]", now);
    if (Objects.isNull(t.getCreateTime())) {
      t.setCreateTime(now);
    }
    if (Objects.isNull(t.getUpdateTime())) {
      t.setUpdateTime(now);
    }
    t.setId(AppContextInject.getBean(SnowflakeIdWorker.class).nextId());
    getRepository().insert(t);
    return t;
  }

  /**
   * Create batch list.
   *
   * @param ts the ts
   * @return the list
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default List<T> createBatch(List<T> ts) throws RuntimeException {
    if (CollectionUtils.isEmpty(ts)) {
      return Collections.emptyList();
    }
    // TODO TXF 更新为最新的批量插入

    /// 可能会参考以下代码
    //    final SnowflakeIdWorker idWorker = AppContextInject.getBean(SnowflakeIdWorker.class);
    //    final Date now = new Date();
    //    log.info("create:新增数据时间[{}]", now);
    //    ts.forEach(
    //        t -> {
    //          textFilter(t, t);
    //          if (Objects.isNull(t.getCreateTime())) {
    //            t.setCreateTime(now);
    //          }
    //          if (Objects.isNull(t.getUpdateTime())) {
    //            t.setUpdateTime(now);
    //          }
    //          t.setId(idWorker.nextId());
    //        });
    //    final long count = AppContextInject.getBean(Update.class).create(ts);
    //    log.info(
    //        "createBatch:[{},{},{},{}]",
    //        count,
    //        ts.size(),
    //        count == ts.size(),
    //        Objects.equals(count, ts.size()));
    //    if (count != ts.size()) {
    //      throw CommonException.of(ResultCode.API_FAIL_EXEC_ADD);
    //    }

    ts.forEach(t -> textFilter(t, t));
    String sqlStatement = sqlStatement(SqlMethod.INSERT_ONE);
    int size = ts.size();
    final SnowflakeIdWorker idWorker = AppContextInject.getBean(SnowflakeIdWorker.class);
    final Date now = new Date();
    executeBatch(
        sqlSession -> {
          AtomicInteger i = new AtomicInteger(1);
          ts.forEach(
              t -> {
                if (Objects.isNull(t.getCreateTime())) {
                  t.setCreateTime(now);
                }
                if (Objects.isNull(t.getUpdateTime())) {
                  t.setUpdateTime(now);
                }
                t.setId(idWorker.nextId());
                sqlSession.insert(sqlStatement, t);
                if ((i.get() % BATCH_SIZE == 0) || i.get() == size) {
                  sqlSession.flushStatements();
                }
                i.getAndIncrement();
              });
        });
    return ts;
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
    return Update.getInstance().from(EntityUtils.getPoJoClass(this)).where(consumer).delete();
  }
}
