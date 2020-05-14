package io.github.ramerf.wind.core.service;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.enums.SqlMethod;
import com.baomidou.mybatisplus.core.toolkit.*;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.repository.BaseRepository;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.io.Serializable;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import org.apache.ibatis.reflection.ExceptionUtil;
import org.apache.ibatis.session.*;
import org.mybatis.spring.MyBatisExceptionTranslator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.transaction.annotation.Transactional;

import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.COLUMN_COMPANY_ID;
import static io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo.COLUMN_ID;
import static io.github.ramerf.wind.core.util.BeanUtils.getPoJoClass;
import static io.github.ramerf.wind.core.util.BeanUtils.initial;
import static org.springframework.util.Assert.notNull;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020 /1/5
 */
@SuppressWarnings({"unused", "rawtypes", "DuplicatedCode"})
public interface InterService<T extends AbstractEntityPoJo> {
  /** The constant log. */
  Logger log = LoggerFactory.getLogger(InterService.class);

  /**
   * Gets by id.
   *
   * @param id the id
   * @return the by id
   */
  default T getById(final long id) {
    return getRepository().selectById(id);
  }

  /**
   * List by ids list.
   *
   * @param ids the ids
   * @return the list
   */
  default List<T> listByIds(final Collection<? extends Serializable> ids) {
    if (CollectionUtils.isEmpty(ids)) {
      return Collections.emptyList();
    }
    return getRepository().selectBatchIds(ids);
  }

  /**
   * 注意: 该方法只会更新不为null的字段..
   *
   * @param t the t
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  // default T update(T t) throws RuntimeException {
  //   final Long companyId = t.getCompanyId();
  //   t.setUpdateTime(new Date());
  //   textFilter(t, t);
  //   UpdateWrapper<T> wrapper = Wrappers.update();
  //   wrapper.eq(COLUMN_ID, t.getId());
  //   wrapper.isNull(Objects.isNull(companyId), COLUMN_COMPANY_ID);
  //   wrapper.eq(Objects.nonNull(companyId), COLUMN_COMPANY_ID, companyId);
  //   return getRepository().update(t, wrapper) > 0 ? t : null;
  // }

  /**
   * 该方法只会更新不为null的字段.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param consumer 入参为实际更新记录数(正常情况下应该返回1,但有些时候根据条件并没有找到记录,这时实际更新记录数会返回0)
   * @throws RuntimeException 更新失败时
   */
  default void update(T t, @Nonnull Consumer<Integer> consumer) throws RuntimeException {
    final Long companyId = t.getCompanyId();
    t.setUpdateTime(new Date());
    textFilter(t, t);
    UpdateWrapper<T> wrapper = Wrappers.update();
    wrapper.eq(COLUMN_ID, t.getId());
    wrapper.isNull(Objects.isNull(companyId), COLUMN_COMPANY_ID);
    wrapper.eq(Objects.nonNull(companyId), COLUMN_COMPANY_ID, companyId);
    consumer.accept(getRepository().update(t, wrapper));
  }

  /**
   * 原方法保留 注意: 该方法只会更新不为null的字段..
   *
   * @param t the t
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T update(T t) throws RuntimeException {
    QueryWrapper<T> wrapper = Wrappers.query();
    wrapper.eq(COLUMN_ID, t.getId());
    wrapper.isNull(Objects.isNull(t.getCompanyId()), COLUMN_COMPANY_ID);
    wrapper.eq(Objects.nonNull(t.getCompanyId()), COLUMN_COMPANY_ID, t.getCompanyId());
    t.setUpdateTime(new Date());
    return Optional.ofNullable(getRepository().selectOne(wrapper))
        .map(
            (o) -> {
              textFilter(t, t);
              return getRepository().updateById(t) > 0 ? t : null;
            })
        .orElse(null);
  }

  /**
   * 注意: 该方法只会更新不为null的字段.
   *
   * @param t the t
   * @param wrapper the wrapper
   * @return the t
   * @throws RuntimeException the runtime exception
   */
  default T update(T t, LambdaUpdateWrapper<T> wrapper) throws RuntimeException {
    if (Objects.isNull(wrapper)) {
      return null;
    }
    t.setUpdateTime(new Date());
    return getRepository().update(t, wrapper) > 0 ? t : null;
  }

  /** The constant batchSize. */
  int BATCH_SIZE = 50;

  /**
   * 批量更新.<br>
   * 注意: 该方法会将ts中的值完全覆盖对应的数据.<br>
   * TODO-WARN: 校验数据是否属于当前企业
   *
   * @param <R> {@link AbstractEntityRequest}
   * @param ts 要更新的数据集
   * @param includeNullProperties the include null properties
   * @return {@link AbstractEntityPoJo}
   * @throws RuntimeException sqlException
   */
  @Transactional(rollbackFor = Exception.class)
  default <R extends AbstractEntityRequest> List<T> updateBatch(
      List<R> ts, final String... includeNullProperties) throws RuntimeException {
    if (CollectionUtils.isEmpty(ts)) {
      return Collections.emptyList();
    }
    final Map<Long, List<R>> idObjs =
        ts.stream().collect(Collectors.groupingBy(AbstractEntityRequest::getId));
    final List<T> tsDb = new ArrayList<>();
    String sqlStatement = sqlStatement(SqlMethod.UPDATE_BY_ID);
    int size = ts.size();
    executeBatch(
        sqlSession -> {
          AtomicInteger i = new AtomicInteger(1);
          ts.forEach(
              t -> {
                T td = initial(getPoJoClass(this));
                notNull(td, "无法实例化PoJo对象");
                BeanUtils.copyProperties(t, td);
                td.setUpdateTime(new Date());
                sqlSession.update(sqlStatement, Collections.singletonMap(Constants.ENTITY, td));
                tsDb.add(td);
                if ((i.get() % BATCH_SIZE == 0) || i.get() == size) {
                  sqlSession.flushStatements();
                }
                i.getAndIncrement();
              });
        });
    return tsDb;
  }

  /**
   * 批量更新.<br>
   * 注意: 该方法会将ts中的值完全覆盖对应的数据.<br>
   * TODO-WARN: 校验数据是否属于当前企业
   *
   * @param ts 要更新的数据集
   * @param includeNullProperties the include null properties
   * @return {@link AbstractEntityPoJo}
   * @throws RuntimeException sqlException
   */
  @Transactional(rollbackFor = Exception.class)
  default List<T> updateBatchPoJo(List<T> ts, final String... includeNullProperties)
      throws RuntimeException {
    if (CollectionUtils.isEmpty(ts)) {
      return Collections.emptyList();
    }
    final List<T> tsDb = new ArrayList<>();
    String sqlStatement = sqlStatement(SqlMethod.UPDATE_BY_ID);
    int size = ts.size();
    executeBatch(
        sqlSession -> {
          AtomicInteger i = new AtomicInteger(1);
          ts.forEach(
              t -> {
                T td = initial(getPoJoClass(this));
                BeanUtils.copyProperties(t, td);
                td.setUpdateTime(new Date());
                sqlSession.update(sqlStatement, Collections.singletonMap(Constants.ENTITY, td));
                tsDb.add(td);
                if ((i.get() % BATCH_SIZE == 0) || i.get() == size) {
                  sqlSession.flushStatements();
                }
                i.getAndIncrement();
              });
        });
    return tsDb;
  }

  /**
   * Delete.
   *
   * @param id the id
   * @throws RuntimeException the runtime exception
   */
  default void delete(final long id) throws RuntimeException {
    getRepository().deleteById(id);
  }

  /**
   * Delete.
   *
   * @param id the id
   * @param companyId the company id
   * @throws RuntimeException the runtime exception
   */
  default void delete(final long id, final long companyId) throws RuntimeException {
    getRepository().delete(Wrappers.<T>query().eq("id", id).eq("company_id", companyId));
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default void deleteBatch(final List<Long> ids) throws RuntimeException {
    if (CollectionUtils.isEmpty(ids)) {
      return;
    }
    if (getRepository().deleteBatchIds(ids) != ids.size()) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_DELETE);
    }
  }

  /**
   * Delete batch ops.
   *
   * @param ids the ids
   * @return 删除记录数 int
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default int deleteBatchOps(final List<Long> ids) throws RuntimeException {
    if (CollectionUtils.isEmpty(ids)) {
      return 0;
    }
    return getRepository().deleteBatchIds(ids);
  }

  /**
   * Delete batch.
   *
   * @param ids the ids
   * @param companyId the company id
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default void deleteBatch(final List<Long> ids, final long companyId) throws RuntimeException {
    if (!CollectionUtils.isEmpty(ids)) {
      final List<T> ts = listByIds(ids);
      if (ts.stream().allMatch(t -> Objects.equals(t.getCompanyId(), companyId))) {
        deleteBatch(ids);
      }
    }
  }

  /**
   * 条件删除.不支持删除所有记录.
   *
   * @param wrapper the wrapper
   * @return the int
   * @throws RuntimeException the runtime exception
   */
  @Transactional(rollbackFor = Exception.class)
  default int delete(@Nonnull final LambdaUpdateWrapper<T> wrapper) throws RuntimeException {
    return getRepository().delete(wrapper);
  }

  /**
   * 过滤某些属性可能包含的特殊字符.
   *
   * @param trans 页面传递过来的对象
   * @param filtered 过滤后的对象
   */
  default void textFilter(T trans, T filtered) {}

  /**
   * 执行批量操作
   *
   * @param consumer consumer
   * @since 3.3.0
   */
  default void executeBatch(Consumer<SqlSession> consumer) {
    Class<T> tClass = getPoJoClass(this);
    SqlHelper.clearCache(tClass);
    SqlSessionFactory sqlSessionFactory = SqlHelper.sqlSessionFactory(tClass);
    SqlSession sqlSession = sqlSessionFactory.openSession(ExecutorType.BATCH);
    try {
      consumer.accept(sqlSession);
      sqlSession.commit();
    } catch (Throwable t) {
      sqlSession.rollback();
      Throwable unwrapped = ExceptionUtil.unwrapThrowable(t);
      if (unwrapped instanceof RuntimeException) {
        MyBatisExceptionTranslator myBatisExceptionTranslator =
            new MyBatisExceptionTranslator(
                sqlSessionFactory.getConfiguration().getEnvironment().getDataSource(), true);
        throw Objects.requireNonNull(
            myBatisExceptionTranslator.translateExceptionIfPossible((RuntimeException) unwrapped));
      }
      throw ExceptionUtils.mpe(unwrapped);
    } finally {
      sqlSession.close();
    }
  }

  /**
   * Sql statement string.
   *
   * @param sqlMethod the sql method
   * @return the string
   */
  default String sqlStatement(SqlMethod sqlMethod) {
    return SqlHelper.table(getPoJoClass(this)).getSqlStatement(sqlMethod.getMethod());
  }

  /**
   * Gets query column.
   *
   * @return the query column
   */
  default QueryColumn<T> getQueryColumn() {
    return QueryColumnFactory.getInstance(getPoJoClass(this));
  }

  /**
   * Gets query.
   *
   * @return the query
   */
  default Query getQuery() {
    return Query.getInstance();
  }

  /**
   * Gets update.
   *
   * @return the update
   */
  default Update getUpdate() {
    return getUpdate(false);
  }

  /**
   * Gets update for current.
   *
   * @param current 是否当前类的更新组件
   * @return the update
   */
  default Update getUpdate(final boolean current) {
    final Update instance = Update.getInstance();
    return current ? instance.from(getPoJoClass(this)) : instance;
  }

  /**
   * Gets repository.
   *
   * @param <U> the type parameter
   * @return the repository
   * @throws RuntimeException the runtime exception
   */
  <U extends BaseRepository<T, Long>> U getRepository() throws RuntimeException;
}
