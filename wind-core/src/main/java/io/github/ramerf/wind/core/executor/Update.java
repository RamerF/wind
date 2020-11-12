package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.service.UpdateService.Fields;
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

import static java.util.stream.Collectors.joining;

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
  private ICondition<?> condition;

  private EntityInfo entityInfo;

  private static Executor executor;
  private static WindConfiguration configuration;
  private static IdGenerator idGenerator;
  private static Dialect dialect;

  public static void initial(
      final Executor executor,
      final WindConfiguration configuration,
      final IdGenerator idGenerator,
      final Dialect dialect) {
    Update.executor = executor;
    Update.configuration = configuration;
    Update.idGenerator = idGenerator;
    Update.dialect = dialect;
  }

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
    this.clazz = clazz;
    if (clazz == null) {
      this.condition = QueryColumnFactory.fromTableName(tableName).getCondition();
      this.entityInfo = EntityInfo.of(configuration);
      this.entityInfo.setName(tableName);
    } else {
      this.condition = QueryColumnFactory.fromClass(clazz).getCondition();
      this.entityInfo = EntityHelper.getEntityInfo(clazz);
    }
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
  public <T extends AbstractEntityPoJo> Update lambdaWhere(
      @Nonnull final Consumer<Condition<T>> consumer) {
    consumer.accept((Condition<T>) this.condition);
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
  public <T extends AbstractEntityPoJo> Update strWhere(
      @Nonnull final Consumer<StringCondition<T>> consumer) {
    consumer.accept((StringCondition<T>) this.condition);
    return this;
  }

  /**
   * 创建,默认不保存值为null的列.
   *
   * @param <T> the type parameter
   * @param t the t
   * @throws DataAccessException 如果执行失败
   * @throws CommonException 创建记录条数不等于1
   */
  public <T extends AbstractEntityPoJo> void create(@Nonnull final T t) throws DataAccessException {
    createWithNull(t, null);
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
  public <T extends AbstractEntityPoJo> void createWithNull(
      @Nonnull final T t, List<IFunction<T, ?>> includeNullProps) throws DataAccessException {
    t.setId(idGenerator.nextId(t));
    setCurrentTime(t, entityInfo.getCreateTimeField(), false);
    setCurrentTime(t, entityInfo.getUpdateTimeField());
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
      t.setId((Long) Objects.requireNonNull(keyHolder.getKeys()).get(dialect.getKeyHolderKey()));
    }
    if (update != 1 || Objects.isNull(t.getId())) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_CREATE);
    }
  }

  private <T extends AbstractEntityPoJo> void setCurrentTime(
      @Nonnull final T t, final Field field) {
    setCurrentTime(t, field, true);
  }

  private <T extends AbstractEntityPoJo> void setCurrentTime(
      @Nonnull final T t, final Field field, final boolean isUpdateTime) {
    if (field == null) {
      return;
    }
    final Object val = BeanUtils.getValue(t, field, null);
    // 只考虑了有限的情况,如果使用了基本类型long,默认值为0,此时也需要赋值
    if (val == null || (val instanceof Long && (Long) val < 1)) {
      final Object value;
      if (Date.class.isAssignableFrom(field.getType())) {
        value = new Timestamp(System.currentTimeMillis());
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
   * @return 保存成功数
   */
  public <T extends AbstractEntityPoJo> Optional<Integer> createBatch(final List<T> ts) {
    return createBatchWithNull(ts, null);
  }

  /**
   * 批量创建,默认不保存null值.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return 保存成功数
   */
  public <T extends AbstractEntityPoJo> Optional<Integer> createBatchWithNull(
      final List<T> ts, List<IFunction<T, ?>> includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 取第一条记录获取批量保存sql
    final T t = ts.get(0);
    ts.forEach(
        o -> {
          setCurrentTime(o, entityInfo.getCreateTimeField(), false);
          setCurrentTime(o, entityInfo.getUpdateTimeField());
          o.setId(idGenerator.nextId(o));
        });
    final Set<Field> savingFields = getSavingFields(t, includeNullProps);
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    savingFields.forEach(
        field -> {
          final String column = EntityUtils.fieldToColumn(field);
          columns.append(columns.length() > 0 ? ",".concat(column) : column);
          valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
        });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql =
        String.format(sql, entityInfo.getName(), columns.toString(), valueMarks.toString());

    AtomicInteger createRow = new AtomicInteger();
    BatchExecUtil.batchExec(
        ts,
        configuration.getBatchSize(),
        execList -> {
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

          /*
           * From java.sql.Statement#executeBatch().
           *
           * Submits a batch of commands to the database for execution and if all commands execute
           * successfully, returns an array of update counts. The <code>int</code> elements of the
           * array that is returned are ordered to correspond to the commands in the batch, which
           * are ordered according to the order in which they were added to the batch. The elements
           * in the array returned by the method <code>executeBatch</code> may be one of the
           * following:
           *
           * <OL>
           *   <LI>A number greater than or equal to zero -- indicates that the command was
           *       processed successfully and is an update count giving the number of rows in the
           *       database that were affected by the command's execution
           *   <LI>A value of <code>SUCCESS_NO_INFO</code> -- indicates that the command was
           *       processed successfully but that the number of rows affected is unknown
           *       <p>If one of the commands in a batch update fails to execute properly, this
           *       method throws a <code>BatchUpdateException</code>, and a JDBC driver may or may
           *       not continue to process the remaining commands in the batch. However, the
           *       driver's behavior must be consistent with a particular DBMS, either always
           *       continuing to process commands or never continuing to process commands. If the
           *       driver continues processing after a failure, the array returned by the method
           *       <code>BatchUpdateException.getUpdateCounts</code> will contain as many elements
           *       as there are commands in the batch, and at least one of the elements will be the
           *       following:
           *   <LI>A value of <code>EXECUTE_FAILED</code> -- indicates that the command failed to
           *       execute successfully and occurs only if a driver continues to process commands
           *       after a command fails
           * </OL>
           *
           * <p>
           */
          createRow.addAndGet(
              Arrays.stream(batchUpdate).filter(o -> o >= 0 || o == -2).map(o -> 1).sum());
        });
    return createRow.get() == ts.size() ? Optional.empty() : Optional.of(createRow.get());
  }

  /**
   * 更新,默认根据id更新且不更新值为null的列.
   *
   * @param <T> the type parameter
   * @param t the t
   * @return 受影响记录数
   */
  public <T extends AbstractEntityPoJo> int update(@Nonnull final T t) {
    return updateWithNull(t, null);
  }

  /**
   * 更新,默认根据id更新且不更新值为null的列.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return 受影响记录数
   */
  @SuppressWarnings("DuplicatedCode")
  public <T extends AbstractEntityPoJo> int updateWithNull(
      @Nonnull final T t, List<IFunction<T, ?>> includeNullProps) {
    setCurrentTime(t, entityInfo.getUpdateTimeField());
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
      lambdaWhere(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
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
   * @return the int
   */
  @SuppressWarnings("DuplicatedCode")
  public <T extends AbstractEntityPoJo> Optional<Integer> updateBatch(@Nonnull final List<T> ts) {
    return updateBatchWithNull(ts, null);
  }

  /**
   * 批量更新,根据id更新.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return the int
   */
  @SuppressWarnings("DuplicatedCode")
  public <T extends AbstractEntityPoJo> Optional<Integer> updateBatchWithNull(
      @Nonnull final List<T> ts, List<IFunction<T, ?>> includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 保存更新时间
    ts.forEach(o -> setCurrentTime(o, entityInfo.getUpdateTimeField()));

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
    lambdaWhere(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder.toString(), condition.getString());

    AtomicInteger updateRow = new AtomicInteger();
    BatchExecUtil.batchExec(
        ts,
        configuration.getBatchSize(),
        execList -> {
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
                      setCurrentTime(obj, entityInfo.getUpdateTimeField());
                      savingFields.forEach(
                          field ->
                              setArgsValue(index, field, BeanUtils.getValue(obj, field, null), ps));
                      Condition.of(QueryColumnFactory.fromClass((Class<T>) clazz))
                          .eq(T::setId, obj.getId())
                          .getValues(index)
                          .forEach(val -> val.accept(ps));
                    }

                    @Override
                    public int getBatchSize() {
                      return execList.size();
                    }
                  });
          updateRow.getAndAdd(
              Arrays.stream(batchUpdate).filter(o -> o >= 0 || o == -2).map(o -> 1).sum());
        });
    return updateRow.get() == ts.size() ? Optional.empty() : Optional.of(updateRow.get());
  }

  /**
   * 更新指定字段,如果条件为空,根据id更新,如果未指定字段,更新不为null的属性.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param fields 更新字段
   * @return 受影响记录数 int
   */
  public <T extends AbstractEntityPoJo> int updateField(
      final T t, final Consumer<Fields<T>> fields) {
    setCurrentTime(t, entityInfo.getUpdateTimeField());
    final Fields<T> entityFields = new Fields<>();
    fields.accept(entityFields);
    final List<IFunction<T, ?>> fieldFunctions = entityFields.getIncludeFields();
    if (fieldFunctions.size() == 0) {
      // 未指定字段,更新不为null的属性
      return update(t);
    }
    final String setString =
        fieldFunctions.stream()
            .map(fieldFunction -> fieldFunction.getColumn() + "=?")
            .collect(joining(","));
    // 没有条件时,默认根据id更新
    if (condition.isEmpty()) {
      if (Objects.isNull(t.getId())) {
        throw new IllegalArgumentException("id could not be null");
      }
      lambdaWhere(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
    }
    final String sql = "update %s set %s where %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setString, condition.getString());
    return executor.update(
        clazz,
        execSql,
        ps -> {
          for (int i = 0; i < fieldFunctions.size(); i++) {
            ps.setObject(i + 1, fieldFunctions.get(i).apply(t));
          }
          condition
              .getValues(new AtomicInteger(fieldFunctions.size() + 1))
              .forEach(val -> val.accept(ps));
        });
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
    // 如果不支持逻辑删除
    if (!entityInfo.getLogicDeleteProp().isEnable()) {
      final String delSql = "delete from %s where %s";
      return executor.update(
          clazz,
          String.format(delSql, entityInfo.getName(), condition.getString()),
          ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
    }
    // 执行逻辑删除
    final Field updateTimeField = entityInfo.getUpdateTimeField();
    final boolean containUpdateTime = updateTimeField != null;
    // 包含更新时间
    if (containUpdateTime) {
      final String updateString =
          String.format(
              "update %s set %s=%s,%s=? where %s",
              entityInfo.getName(),
              entityInfo.getLogicDeleteProp().getColumn(),
              entityInfo.getLogicDeleteProp().isDeleted(),
              entityInfo.getFieldColumnMap().get(updateTimeField.getName()),
              condition.getString());
      return executor.update(
          clazz,
          updateString,
          ps -> {
            if (Date.class.isAssignableFrom(updateTimeField.getType())) {
              ps.setTimestamp(1, new Timestamp(System.currentTimeMillis()));
            } else {
              ps.setLong(1, System.currentTimeMillis());
            }
            condition.getValues(new AtomicInteger(2)).forEach(val -> val.accept(ps));
          });
    } else {
      final String updateString =
          String.format(
              "update %s set %s=%s where %s",
              entityInfo.getName(),
              entityInfo.getLogicDeleteProp().getColumn(),
              entityInfo.getLogicDeleteProp().isDeleted(),
              condition.getString());
      return executor.update(
          clazz,
          updateString,
          ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
    }
  }

  /**
   * 获取要保存的属性.
   *
   * @param t 需要保存的对象
   * @param includeNullProps 即使值为null也需要保存的属性
   * @param <T> the type parameter
   * @return 要保存的属性
   */
  private <T extends AbstractEntityPoJo> Set<Field> getSavingFields(
      final T t, List<IFunction<T, ?>> includeNullProps) {
    final Set<Field> savingFields = new HashSet<>(EntityUtils.getNonNullColumnFields(t));
    if (CollectionUtils.nonEmpty(includeNullProps)) {
      includeNullProps.forEach(prop -> savingFields.add(prop.getField()));
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
