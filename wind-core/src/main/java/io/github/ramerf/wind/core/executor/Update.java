package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.Condition;
import io.github.ramerf.wind.core.condition.ICondition;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.Date;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;

/**
 * 通用写入操作对象.获取实例:<br>
 *
 * <pre>
 *     // 方式1:
 *     <code>@Resource private Provider&lt;Update&gt; updateProvider;</code>
 *     // 方式2:
 *     <code>@Resource private ObjectProvider&lt;Query&gt; updateProvider;</code>
 *     final Update update = updateProvider.get();
 *     // 方式3:
 *     <code>@Resource private PrototypeBean prototypeBean;</code>
 *     final Update update = prototypeBean.update();
 *   </pre>
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/13
 */
@Slf4j
@SuppressWarnings("unused")
public final class Update {

  private Class<?> clazz;
  private Condition<?> condition;

  private EntityInfo entityInfo;

  public static Executor executor;

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static Update getInstance() {
    return AppContextInject.getBean(Update.class);
  }

  /**
   * From update.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the update
   */
  public <T extends AbstractEntityPoJo> Update from(final Class<T> clazz) {
    return from(clazz, null);
  }

  /**
   * From update.
   *
   * @param tableName the table name
   * @return the update
   */
  public Update from(final String tableName) {
    return from(null, tableName);
  }

  private <T extends AbstractEntityPoJo> Update from(final Class<T> clazz, final String tableName) {
    if (Objects.isNull(clazz) && StringUtils.isEmpty(tableName)) {
      throw CommonException.of("[clazz,tableName]不能同时为空");
    }
    this.condition = QueryColumnFactory.getInstance(clazz, tableName, null).getCondition();
    this.clazz = clazz;
    this.entityInfo = condition.getEntityInfo();
    return this;
  }

  /**
   * Where update.
   *
   * @param <T> the type parameter
   * @param consumer the consumer
   * @return the update
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntityPoJo> Update where(
      @Nonnull final Consumer<ICondition<T>> consumer) {
    consumer.accept((ICondition<T>) this.condition);
    return this;
  }

  /**
   * 创建,默认不保存值为null的列.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  @SafeVarargs
  public final <T extends AbstractEntityPoJo> void create(
      @Nonnull final T t, final IFunction<T, ?>... includeNullProps) throws DataAccessException {
    t.setId(AppContextInject.getBean(IdGenerator.class).nextId(t));
    setCurrentTime(t, entityInfo.getCreateTimeField());
    setCurrentTime(t, entityInfo.getUpdateTimeFiled());
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    final AtomicInteger index = new AtomicInteger(1);
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, includeNullProps)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              columns.append(columns.length() > 0 ? ",".concat(column) : column);
              valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
              getArgsValueSetConsumer(index, field, BeanUtils.getValue(t, field, null), list);
            });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql =
        String.format(sql, entityInfo.getName(), columns.toString(), valueMarks.toString());
    KeyHolder keyHolder = new GeneratedKeyHolder();
    final int update =
        executor.update(
            clazz,
            con -> {
              final PreparedStatement ps =
                  con.prepareStatement(execSql, Statement.RETURN_GENERATED_KEYS);
              list.forEach(val -> val.accept(ps));
              return ps;
            },
            keyHolder);
    if (Objects.isNull(t.getId())) {
      t.setId((Long) Objects.requireNonNull(keyHolder.getKeys()).get("id"));
    }
    if (update != 1 || Objects.isNull(t.getId())) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_CREATE);
    }
  }

  private <T extends AbstractEntityPoJo> void setCurrentTime(
      @Nonnull final T t, final Field field) {
    final Object val = BeanUtils.getValue(t, field, null);
    // 只考虑了有限的情况,如果使用了基本类型long,默认值为0,此时也需要赋值
    if (val == null || (val instanceof Long && (Long) val < 1)) {
      final Object value;
      if (Date.class.isAssignableFrom(field.getType())) {
        value = new Date();
      } else {
        value = System.currentTimeMillis();
      }
      BeanUtils.setValue(t, field, value, null);
    }
  }

  /**
   * 批量创建,默认不保存null值.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return 保存成功数
   */
  @SafeVarargs
  public final <T extends AbstractEntityPoJo> int createBatch(
      final List<T> ts, @Nonnull final IFunction<T, ?>... includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    // 取第一条记录获取批量保存sql
    final T t = ts.get(0);
    final IdGenerator idGenerator = AppContextInject.getBean(IdGenerator.class);
    ts.forEach(o -> o.setId(idGenerator.nextId(t)));
    final Set<Field> savingFields = getSavingFields(t, includeNullProps);
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    savingFields.forEach(
        field -> {
          final String column = EntityUtils.fieldToColumn(field);
          columns.append(columns.length() > 0 ? ",".concat(column) : column);
          valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
        });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql =
        String.format(sql, entityInfo.getName(), columns.toString(), valueMarks.toString());
    int createRow = 0;
    final int batchSize = AppContextInject.getBean(WindConfiguration.class).getBatchSize();
    int total = ts.size();
    final int execCount = total / batchSize + (total % batchSize != 0 ? 1 : 0);
    for (int j = 0; j < execCount; j++) {
      final List<T> execList = ts.subList(j * batchSize, Math.min((j + 1) * batchSize, total));
      final int[] batchUpdate =
          executor.batchUpdate(
              clazz,
              execSql,
              new BatchPreparedStatementSetter() {
                @Override
                public void setValues(@Nonnull final PreparedStatement ps, final int i) {
                  final AtomicInteger index = new AtomicInteger(1);
                  final T obj = execList.get(i);
                  obj.setCreateTime(new Date());
                  savingFields.forEach(
                      field ->
                          setArgsValue(index, field, BeanUtils.getValue(obj, field, null), ps));
                }

                @Override
                public int getBatchSize() {
                  return execList.size();
                }
              });
      createRow += Arrays.stream(batchUpdate).filter(o -> o == 1).sum();
    }
    return createRow;
  }

  /**
   * 更新,默认根据id更新且不更新值为null的列.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return 受影响记录数
   */
  @SafeVarargs
  @SuppressWarnings("DuplicatedCode")
  public final <T extends AbstractEntityPoJo> int update(
      @Nonnull final T t, final IFunction<T, ?>... includeNullProps) {
    setCurrentTime(t, entityInfo.getUpdateTimeFiled());
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger(1);
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, includeNullProps)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              setBuilder.append(String.format(setBuilder.length() > 0 ? ",%s=?" : "%s=?", column));
              getArgsValueSetConsumer(index, field, BeanUtils.getValue(t, field, null), list);
            });
    // 没有条件时,默认根据id更新
    if (condition.isEmpty()) {
      if (Objects.isNull(t.getId())) {
        throw new IllegalArgumentException("id could not be null");
      }
      where(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
    }
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder.toString(), condition.getString());
    return executor.update(
        clazz,
        execSql,
        ps -> {
          list.forEach(consumer -> consumer.accept(ps));
          condition.getValues(index).forEach(val -> val.accept(ps));
        });
  }

  /**
   * 批量更新,根据id更新.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return the int
   */
  @SafeVarargs
  @SuppressWarnings("DuplicatedCode")
  public final <T extends AbstractEntityPoJo> Optional<Integer> updateBatch(
      @Nonnull final List<T> ts, final IFunction<T, ?>... includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 取第一条记录获取批量更新sql
    final T t = ts.get(0);
    final Set<Field> savingFields = getSavingFields(t, includeNullProps);
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    savingFields.forEach(
        field ->
            setBuilder.append(
                String.format(
                    setBuilder.length() > 0 ? ",%s=?" : "%s=?", EntityUtils.fieldToColumn(field))));
    if (Objects.isNull(t.getId())) {
      throw new IllegalArgumentException("id could not be null");
    }
    // 保证占位符对应
    where(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder.toString(), condition.getString());
    int updateRow = 0;
    final int batchSize = AppContextInject.getBean(WindConfiguration.class).getBatchSize();
    int total = ts.size();
    final int execCount = total / batchSize + (total % batchSize != 0 ? 1 : 0);
    for (int j = 0; j < execCount; j++) {
      final List<T> execList = ts.subList(j * batchSize, Math.min((j + 1) * batchSize, total));
      final int[] batchUpdate =
          executor.batchUpdate(
              clazz,
              execSql,
              new BatchPreparedStatementSetter() {
                @Override
                @SuppressWarnings("unchecked")
                public void setValues(@Nonnull final PreparedStatement ps, final int i) {
                  final AtomicInteger index = new AtomicInteger(1);
                  final T obj = execList.get(i);
                  setCurrentTime(t, entityInfo.getUpdateTimeFiled());
                  savingFields.forEach(
                      field ->
                          setArgsValue(index, field, BeanUtils.getValue(obj, field, null), ps));
                  Condition.of(QueryColumnFactory.getInstance((Class<T>) clazz))
                      .eq(T::setId, obj.getId())
                      .getValues(index)
                      .forEach(val -> val.accept(ps));
                }

                @Override
                public int getBatchSize() {
                  return execList.size();
                }
              });
      updateRow += Arrays.stream(batchUpdate).filter(o -> o == 1).sum();
    }
    return updateRow == ts.size() ? Optional.empty() : Optional.of(updateRow);
  }

  /**
   * Update batch request int.
   *
   * @param <R> the type parameter
   * @param ts the ts
   * @param includeNull the include null
   * @return the int
   */
  public <R extends AbstractEntityRequest<?>> int updateBatchRequest(
      @Nonnull final List<R> ts, final boolean includeNull) {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    throw CommonException.of(ResultCode.API_NOT_IMPLEMENT);
  }

  /**
   * 条件删除,不允许条件为空.
   *
   * @return 删除记录数 int
   * @throws DataAccessException 如果执行失败
   */
  public int delete() throws DataAccessException {
    // 不包含删除条件,抛异常
    if (condition.isEmpty()) {
      throw CommonException.of(ResultCode.API_FAIL_DELETE_NO_CONDITION);
    }
    final String sql = "update %s set %s=%s,%s='%s' where %s";
    final Field updateTimeFiled = entityInfo.getUpdateTimeFiled();
    final String updateString =
        String.format(
            sql,
            entityInfo.getName(),
            entityInfo.getLogicDeleteProp().getColumn(),
            entityInfo.getLogicDeleteProp().isDeleted(),
            entityInfo.getFieldColumnMap().get(updateTimeFiled.getName()),
            Date.class.isAssignableFrom(updateTimeFiled.getType())
                ? new Date()
                : System.currentTimeMillis(),
            condition.getString());
    return executor.update(
        clazz,
        updateString,
        ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
  }

  /**
   * 获取要保存的属性.
   *
   * @param t 需要保存的对象
   * @param includeNullProps 即使值为null也需要保存的属性
   * @param <T> the type parameter
   * @return 要保存的属性
   */
  @SafeVarargs
  private final <T extends AbstractEntity> Set<Field> getSavingFields(
      final T t, final IFunction<T, ?>... includeNullProps) {
    final Set<Field> savingFields = new HashSet<>(EntityUtils.getNonNullColumnFields(t));
    if (Objects.nonNull(includeNullProps) && includeNullProps.length > 0) {
      Arrays.stream(includeNullProps).forEach(prop -> savingFields.add(prop.getField()));
    }
    return savingFields;
  }

  /**
   * 获取参数值对应的{@link PreparedStatement#setObject(int, Object)}.
   *
   * @param index 起始索引
   * @param field 对应字段
   * @param originValue 原始值
   * @param list 设置参数值的consumer集合
   */
  private void getArgsValueSetConsumer(
      AtomicInteger index,
      Field field,
      Object originValue,
      List<Consumer<PreparedStatement>> list) {
    Consumer<PreparedStatement> function =
        ps -> {
          try {
            final Object value =
                TypeHandlerHelper.toJdbcValue(ValueType.of(originValue, field), ps);
            if (log.isDebugEnabled()) {
              log.debug(
                  "setParameterConsumer:[index:{},originValue:{},value:{}]",
                  index.get(),
                  originValue,
                  value);
            }
            ps.setObject(index.getAndIncrement(), value);
          } catch (SQLException e) {
            throw CommonException.of(e);
          }
        };
    list.add(function);
  }

  /**
   * 设置参数值.
   *
   * @param index 起始索引
   * @param field 对应字段
   * @param originValue 原始值
   * @param ps the {@link PreparedStatement}
   */
  private void setArgsValue(
      AtomicInteger index, Field field, Object originValue, PreparedStatement ps) {
    final Object value = TypeHandlerHelper.toJdbcValue(ValueType.of(originValue, field), ps);
    if (log.isTraceEnabled()) {
      log.trace("setArgsValue:[index:{},originValue:{},value:{}]", index.get(), originValue, value);
    }
    try {
      ps.setObject(index.getAndIncrement(), value);
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      throw CommonException.of(e.getMessage(), e);
    }
  }
}
