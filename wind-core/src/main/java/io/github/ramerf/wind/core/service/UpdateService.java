package io.github.ramerf.wind.core.service;

import com.baomidou.mybatisplus.core.enums.SqlMethod;
import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.condition.Update;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.SnowflakeIdWorker;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import static io.github.ramerf.wind.core.util.BeanUtils.*;

/**
 * The interface Update service.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings("rawtypes")
public interface UpdateService<T extends AbstractEntityPoJo, E extends AbstractEntity>
    extends InterService<T, E> {
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
   * 条件删除.
   *
   * @param consumer the consumer.<br>
   *     示例:
   *     <pre>
   *      condition -&gt; condition.eq(AbstractEntityPoJo::getId)
   *     </pre>
   *
   * @return 删除记录数
   * @throws RuntimeException the runtime exception
   * @see DataAccessException
   */
  default long delete(Consumer<ICondition<T>> consumer) throws RuntimeException {
    return Update.getInstance().from(getPoJoClass(this)).where(consumer).delete();
  }

  /**
   * 保存/更新{@link U}对应的Domain对象.默认不会覆盖{@link U}中为null的字段,包含{@code
   * includeNullProperties}**中的属性,即使值为null.
   *
   * @param <U> Request 实体.
   * @param u 页面请求对象 {@link AbstractEntityRequest}.
   * @param includeNullProperties 覆写这些属性值,即使值为null.
   * @return T <br>
   *     null,如果保存/更新失败,或者更新时记录不存在.
   * @throws RuntimeException the runtime exception
   * @see SQLException
   * @see CommonException
   */
  @SuppressWarnings({"unchecked", "DeprecatedIsStillUsed"})
  @Deprecated
  default <U extends AbstractEntityRequest> T createOrUpdate(U u, String... includeNullProperties)
      throws RuntimeException {
    final Long id = u.getId();
    T domain = Objects.isNull(id) ? null : getById(id);
    if (Objects.nonNull(id) && Objects.isNull(domain)) {
      return null;
    }
    domain = Objects.isNull(domain) ? initial(getPoJoClass(this)) : domain;
    BeanUtils.copyProperties(
        u,
        domain,
        getNullProp(u).stream()
            .filter(prop -> !Arrays.asList(includeNullProperties).contains(prop))
            .toArray(String[]::new));
    u.redundantValue(domain);
    return Objects.isNull(id) ? create(domain) : update(domain);
  }
}
