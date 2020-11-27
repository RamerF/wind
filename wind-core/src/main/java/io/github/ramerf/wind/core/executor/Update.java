package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.service.InterService.Fields;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.*;
import java.time.*;
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

import static java.util.stream.Collectors.toList;

/**
 * 通用写入操作对象.
 *
 * <p>获取PoJo的写入实例<br>
 *
 * <p>方式1<br>
 * {@code Update.getInstance(PoJo.class)}
 *
 * <p>方式2<br>
 * {@code @Resource private Provider<Update<PoJo>> updateProvider;}
 *
 * <p>方式3<br>
 * {@code @Resource private ObjectProvider<Update<PoJo>> updateProvider;}<br>
 * final Update<PoJo> update = updateProvider.get();
 *
 * <p>方式4<br>
 * {@code @Resource private PrototypeBean prototypeBean;}<br>
 * final Update<PoJo> update = prototypeBean.update(PoJo.class);
 *
 * @author Tang Xiaofeng
 * @since 2020/1/13
 */
@Slf4j
@SuppressWarnings("unused")
public final class Update<T extends AbstractEntityPoJo<T, ?>> {

  private final Class<T> clazz;
  private Condition<T> condition;
  private Fields<T> fields;
  private final EntityInfo entityInfo;
  private static Executor executor;
  private static WindConfiguration configuration;
  private static IdGenerator idGenerator;
  private static Dialect dialect;
  private static PrototypeBean prototypeBean;
  private final Field idField;
  private Field logicDeleteField;
  private boolean logicDeletedValue;
  private boolean logicNotDeleteValue;

  /**
   * Instantiates a new Update.
   *
   * @param clazz the clazz
   * @since 2020.11.13
   * @author Tang Xiaofeng
   */
  public Update(final Class<T> clazz) {
    this.clazz = clazz;
    this.entityInfo = EntityHelper.getEntityInfo(clazz);
    this.idField = EntityHelper.getEntityIdField(clazz);
    final EntityColumn deletePropColumn = this.entityInfo.getLogicDeletePropColumn();
    if (deletePropColumn != null) {
      logicDeleteField = deletePropColumn.getField();
      final LogicDeleteProp logicDeleteProp = this.entityInfo.getLogicDeleteProp();
      logicDeletedValue = logicDeleteProp.isDeleted();
      logicNotDeleteValue = logicDeleteProp.isNotDelete();
    }
  }

  /**
   * Initial.
   *
   * @param executor the executor
   * @param configuration the configuration
   * @param idGenerator the id generator
   * @param dialect the dialect
   */
  public static void initial(
      final Executor executor,
      final WindConfiguration configuration,
      final IdGenerator idGenerator,
      final Dialect dialect,
      final PrototypeBean prototypeBean) {
    Update.executor = executor;
    Update.configuration = configuration;
    Update.idGenerator = idGenerator;
    Update.dialect = dialect;
    Update.prototypeBean = prototypeBean;
  }

  /**
   * Gets instance.
   *
   * @param <T> the type parameter
   * @param clazz the clazz
   * @return the instance
   */
  public static <T extends AbstractEntityPoJo<T, ?>> Update<T> getInstance(final Class<T> clazz) {
    return prototypeBean.update(clazz);
  }

  /**
   * Where update.
   *
   * @param condition the condition
   * @return the update
   */
  public Update<T> where(@Nonnull final Condition<T> condition) {
    this.condition = condition;
    return this;
  }

  /**
   * 创建,默认不保存值为null的列.
   *
   * @param t the t
   * @return the t
   * @throws DataAccessException 如果执行失败
   */
  public T create(@Nonnull final T t) throws DataAccessException {
    return create(t, null);
  }

  /**
   * 创建.
   *
   * @param t the t
   * @param fields the fields
   * @throws DataAccessException 如果执行失败
   */
  public T create(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    final Object id = idGenerator.nextId(t);
    if (id != null) {
      BeanUtils.setValue(t, idField, id, null);
    }
    // TODO-POST 如果sql ddl 包含default这里就不需要设置
    setCurrentTime(t, entityInfo.getCreateTimeField(), false);
    setCurrentTime(t, entityInfo.getUpdateTimeField(), true);
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    final AtomicInteger index = new AtomicInteger(1);

    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, fields)
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
            connection -> {
              final PreparedStatement ps =
                  connection.prepareStatement(execSql, Statement.RETURN_GENERATED_KEYS);
              list.forEach(val -> val.accept(ps));
              return ps;
            },
            keyHolder);

    BeanUtils.setValue(
        t,
        idField,
        Objects.requireNonNull(keyHolder.getKeys()).get(dialect.getKeyHolderKey()),
        null);
    if (update != 1 || BeanUtils.getValue(t, idField, null) == null) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_CREATE);
    }
    return t;
  }

  /**
   * 批量创建,默认不保存null值.
   *
   * @param ts the ts
   * @return 保存成功数 optional
   */
  public Optional<Integer> createBatch(final List<T> ts) {
    return createBatch(ts, null);
  }

  /**
   * 批量创建,默认不保存null值.
   *
   * @param ts the ts
   * @param fields the fields
   * @return 保存成功数 optional
   */
  public Optional<Integer> createBatch(final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 取第一条记录获取批量保存sql
    ts.forEach(
        t -> {
          setCurrentTime(t, entityInfo.getCreateTimeField(), false);
          setCurrentTime(t, entityInfo.getUpdateTimeField(), true);
          BeanUtils.setValue(t, idField, idGenerator.nextId(t), null);
        });
    final T t = ts.get(0);
    final List<Field> savingFields = getSavingFields(t, fields);
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
        "create batch",
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
                      // TODO-WARN 创建时间戳
                      // obj.setCreateTime(new Date());
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
   * @param t the t
   * @return 受影响记录数 int
   */
  public int update(@Nonnull final T t) {
    return update(t, null);
  }

  /**
   * 更新,默认根据id更新.
   *
   * @param t the t
   * @param fields the fields
   * @return 受影响记录数 int
   */
  @SuppressWarnings("DuplicatedCode")
  public int update(@Nonnull final T t, final Fields<T> fields) {
    setCurrentTime(t, entityInfo.getUpdateTimeField(), true);
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger(1);
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, fields)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              setBuilder.append(String.format(setBuilder.length() > 0 ? ",%s=?" : "%s=?", column));
              getArgsValueSetConsumer(index, field, BeanUtils.getValue(t, field, null), list);
            });
    // 没有条件时,默认根据id更新
    if (condition.isEmpty()) {
      this.condition.eq(idField, BeanUtils.getValue(t, idField, null));
    }
    this.condition.appendLogicNotDelete();
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
   * @param ts the ts
   * @return the int
   */
  @SuppressWarnings("DuplicatedCode")
  public Optional<Integer> updateBatch(@Nonnull final List<T> ts) {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新,根据id更新.
   *
   * @param ts the ts
   * @param fields the fields
   * @return the int
   */
  @SuppressWarnings("DuplicatedCode")
  public Optional<Integer> updateBatch(@Nonnull final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 保存更新时间
    ts.forEach(o -> setCurrentTime(o, entityInfo.getUpdateTimeField(), true));

    // 取第一条记录获取批量更新sql
    final T t = ts.get(0);
    final List<Field> savingFields = getSavingFields(t, fields);
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    savingFields.forEach(
        field ->
            setBuilder.append(
                String.format(
                    setBuilder.length() > 0 ? ",%s=?" : "%s=?", EntityUtils.fieldToColumn(field))));
    // 保证占位符对应
    this.condition.eq(idField, null);
    this.condition.appendLogicNotDelete();
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder.toString(), condition.getString());

    AtomicInteger updateRow = new AtomicInteger();
    BatchExecUtil.batchExec(
        "update batch",
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
                      setCurrentTime(obj, entityInfo.getUpdateTimeField(), true);
                      savingFields.forEach(
                          field ->
                              setArgsValue(index, field, BeanUtils.getValue(obj, field, null), ps));
                      LambdaCondition.getInstance(QueryColumn.fromClass(clazz))
                          .eq(idField, BeanUtils.getValue(obj, idField, null))
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
    this.condition.appendLogicNotDelete();
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
              "update %s set %s=%s, %s=? where %s",
              entityInfo.getName(),
              entityInfo.getLogicDeletePropColumn().getName(),
              entityInfo.getLogicDeleteProp().isDeleted(),
              entityInfo.getFieldColumnMap().get(updateTimeField.getName()),
              condition.getString());
      return executor.update(
          clazz,
          updateString,
          ps -> {
            ps.setObject(1, getUpdateTimeValue(updateTimeField));
            condition.getValues(new AtomicInteger(2)).forEach(val -> val.accept(ps));
          });
    } else {
      final String updateString =
          String.format(
              "update %s set %s=%s where %s",
              entityInfo.getName(),
              entityInfo.getLogicDeletePropColumn().getName(),
              entityInfo.getLogicDeleteProp().isDeleted(),
              condition.getString());
      return executor.update(
          clazz,
          updateString,
          ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
    }
  }

  private List<Field> getSavingFields(final @Nonnull T t, final Fields<T> fields) {
    final List<Field> savingFields;
    if (fields == null) {
      savingFields =
          configuration.isWriteNullProp()
              ? EntityUtils.getAllColumnFields(t.getClass())
              : EntityUtils.getNonNullColumnFields(t);
    } else {
      final List<Field> includeFields = fields.getIncludeFields();
      final List<Field> excludeFields = fields.getExcludeFields();
      if (includeFields.isEmpty()) {
        savingFields =
            (configuration.isWriteNullProp()
                    ? EntityUtils.getAllColumnFields(t.getClass())
                    : EntityUtils.getNonNullColumnFields(t))
                .stream().filter(field -> !excludeFields.contains(field)).collect(toList());
      } else {
        savingFields =
            includeFields.stream()
                .filter(field -> !excludeFields.contains(field))
                .collect(toList());
      }
    }
    return savingFields;
  }

  private void setCurrentTime(@Nonnull final T t, final Field field, final boolean isUpdateTime) {
    if (field == null) {
      return;
    }
    final Object val = BeanUtils.getValue(t, field, null);
    // 只考虑了有限的情况,如果使用了基本类型long,默认值为0,此时也需要赋值
    if (val == null
        || (val instanceof Long && (Long) val == 0)
        || (val instanceof Integer && (Integer) val == 0)) {
      BeanUtils.setValue(t, field, getUpdateTimeValue(field), null);
    }
  }

  private Object getUpdateTimeValue(final Field updateTimeField) {
    final Class<?> fieldType = updateTimeField.getType();
    final Object value;
    if (LocalDate.class.isAssignableFrom(fieldType)) {
      return LocalDate.now();
    }
    if (LocalTime.class.isAssignableFrom(fieldType)) {
      return LocalTime.now();
    }
    if (LocalDateTime.class.isAssignableFrom(fieldType)) {
      return LocalDateTime.now();
    }
    if (Date.class.isAssignableFrom(fieldType)) {
      return new Timestamp(System.currentTimeMillis());
    }
    if (Integer.class.isAssignableFrom(fieldType)) {
      return System.currentTimeMillis() / 1000;
    }
    return System.currentTimeMillis();
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
