package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.CollectionUtils;
import java.lang.reflect.Field;
import java.util.*;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import lombok.Getter;
import org.springframework.dao.DataAccessException;
import org.springframework.transaction.annotation.Transactional;

import static java.util.stream.Collectors.toList;

/**
 * 执行写数据.
 *
 * <p>Note:默认不写入值为null的属性,通过{@link WindConfiguration#isWriteNullProp()}开关
 *
 * @param <T> the type parameter
 * @author Tang Xiaofeng
 * @since 2020/1/5
 * @see WindConfiguration#isWriteNullProp()
 */
public interface UpdateService<T extends AbstractEntityPoJo> extends InterService<T> {

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  default long create(@Nonnull final T t) throws DataAccessException {
    return create(t, null);
  }

  /**
   * 创建记录.
   *
   * @param t the {@link AbstractEntityPoJo}
   * @param fieldsConsumer the fields consumer
   * @return {@code id}
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  default long create(@Nonnull final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    textFilter(t, t);
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) t.getClass());
      fieldsConsumer.accept(fields);
    }
    getUpdate().create(t, fields);
    return t.getId();
  }

  /**
   * 批量创建.
   *
   * @param ts the ts
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> createBatch(final List<T> ts) throws DataAccessException {
    return getUpdate().createBatch(ts);
  }

  /**
   * 批量创建.
   *
   * @param ts the ts
   * @param fieldsConsumer the fields consumer
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  @SuppressWarnings("unchecked")
  default Optional<Integer> createBatch(
      @Nonnull final List<T> ts, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) ts.get(0).getClass());
      fieldsConsumer.accept(fields);
    }
    return getUpdate().createBatch(ts, fields);
  }

  /**
   * 更新.
   *
   * @param t the t
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default int update(final T t) throws DataAccessException {
    return update(t, null, null);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param fieldsConsumer 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int update(final T t, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    return update(t, fieldsConsumer, null);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param consumer 更新条件
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  default int updateByCondition(final T t, final Consumer<Condition<T>> consumer)
      throws DataAccessException {
    return update(t, null, consumer);
  }

  /**
   * 条件更新.
   *
   * @param t the t
   * @param conditionConsumer 更新条件
   * @param fieldsConsumer 更新字段
   * @return 实际受影响的行数 int
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  default int update(
      final T t, Consumer<Fields<T>> fieldsConsumer, final Consumer<Condition<T>> conditionConsumer)
      throws DataAccessException {
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) t.getClass());
      fieldsConsumer.accept(fields);
    }
    return conditionConsumer == null
        ? getUpdate().update(t, fields)
        : getUpdate().where(conditionConsumer).update(t, fields);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts) throws DataAccessException {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新.<br>
   *
   * @param ts 要更新的数据集
   * @param fields 即使值为null也保存的属性
   * @return 当受影响行数等于 {@code ts.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  @SuppressWarnings("unchecked")
  @Transactional(rollbackFor = Exception.class)
  default Optional<Integer> updateBatch(final List<T> ts, final Consumer<Fields<T>> fieldsConsumer)
      throws DataAccessException {
    if (ts.isEmpty()) {
      return Optional.empty();
    }
    Fields<T> fields = null;
    if (fieldsConsumer != null) {
      fields = Fields.with((Class<T>) ts.get(0).getClass());
      fieldsConsumer.accept(fields);
    }
    return getUpdate().updateBatch(ts, fields);
  }

  /**
   * 删除记录.
   *
   * @param id the id
   * @return 实际受影响的行数
   * @throws DataAccessException 如果执行失败
   * @see DataAccessException
   * @see CommonException
   */
  default int delete(final long id) throws DataAccessException {
    return getUpdate().where(condition -> condition.eq(AbstractEntityPoJo::setId, id)).delete();
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
  default int delete(Consumer<Condition<T>> consumer) throws DataAccessException {
    return getUpdate().where(consumer).delete();
  }

  /**
   * 如果删除数量不等于id的大小,将执行失败.
   *
   * @param ids the ids
   * @return 当受影响行数等于 {@code ids.size()}时,{@link Optional#isPresent()}为false.<br>
   *     否则{@link Optional#get()}返回实际受影响的行数
   * @throws DataAccessException 如果执行失败
   */
  default Optional<Integer> deleteByIds(final Collection<Long> ids) throws DataAccessException {
    if (CollectionUtils.isEmpty(ids)) {
      return Optional.empty();
    }
    final int affectRow =
        getUpdate().where(condition -> condition.in(AbstractEntityPoJo::setId, ids)).delete();
    return affectRow == ids.size() ? Optional.empty() : Optional.of(affectRow);
  }

  /** 函数字段集合.指定一个操作应该包含和不包含的字段. */
  class Fields<T> {
    @Getter private final List<IFunction<T, ?>> includes = new ArrayList<>();
    @Getter private final List<IFunction<T, ?>> excludes = new ArrayList<>();

    public static <T extends AbstractEntityPoJo> Fields<T> with(Class<T> clazz) {
      return new Fields<>();
    }

    @SafeVarargs
    public final Fields<T> include(final IFunction<T, ?>... includeFields) {
      this.includes.addAll(Arrays.asList(includeFields));
      return this;
    }

    // TODO-WARN 未实现exclude
    @SafeVarargs
    public final Fields<T> exclude(@Nonnull final IFunction<T, ?>... excludeFields) {
      for (final IFunction<T, ?> function : excludeFields) {
        this.includes.remove(function);
      }
      this.excludes.addAll(Arrays.asList(excludeFields));
      return this;
    }

    public List<Field> getIncludeFields() {
      return includes.isEmpty()
          ? Collections.emptyList()
          : includes.stream().map(BeanFunction::getField).collect(toList());
    }

    public List<Field> getExcludeFields() {
      return excludes.isEmpty()
          ? Collections.emptyList()
          : excludes.stream().map(BeanFunction::getField).collect(toList());
    }
  }
}
