package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.JdbcEnvironment;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.exception.NotAllowedDataAccessException;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.support.IdGenerator;
import io.github.ramerf.wind.core.util.*;
import io.github.ramerf.wind.core.util.EntityUtils.SqlStatementType;
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.sql.*;
import java.time.*;
import java.util.Date;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

import static java.util.stream.Collectors.toList;

/**
 * 通用写入操作对象.
 *
 * <p>获取PoJo的写入实例<br>
 *
 * <p>方式1<br>
 * {@literal Update.getInstance(PoJo.class)}
 *
 * <p>方式2<br>
 * {@literal @Resource private Provider<Update<PoJo>> updateProvider;}
 *
 * <p>方式3<br>
 * {@literal @Resource private ObjectProvider<Update<PoJo>> updateProvider;}<br>
 * final Update<PoJo> update = updateProvider.get();
 *
 * <p>方式4<br>
 * {@literal @Resource private PrototypeBean prototypeBean;}<br>
 * final Update<PoJo> update = prototypeBean.update(PoJo.class);
 *
 * @author ramer
 * @since 2020/1/13
 */
@Slf4j
@SuppressWarnings("unused")
public final class Update<T> {

  private final Class<T> clazz;
  private Condition<T, ?> condition;
  private Fields<T> fields;
  private final IdGenerator idGenerator;
  private final EntityInfo entityInfo;
  private final Executor executor;
  private static Configuration configuration;
  private final Field idField;

  public Update(@Nonnull final Class<T> clazz) {
    this(clazz, true);
  }

  public Update(@Nonnull final Class<T> clazz, final boolean autoCommit) {
    this.clazz = clazz;
    this.entityInfo = EntityHelper.getEntityInfo(clazz);
    this.idField = this.entityInfo.getIdColumn().getField();
    this.condition = LambdaCondition.of(clazz);
    this.idGenerator = this.entityInfo.getIdGenerator();
    final JdbcEnvironment jdbcEnvironment = configuration.getJdbcEnvironment();
    this.executor =
        new SimpleJdbcExecutor(
            configuration,
            jdbcEnvironment
                .getTransactionFactory()
                .newTransaction(jdbcEnvironment.getDataSource(), autoCommit));
  }

  public static void initial(final Configuration configuration) {
    Update.configuration = configuration;
  }

  public static <T> Update<T> getInstance(final Class<T> clazz) {
    return new Update<>(clazz);
  }

  public static <T> Update<T> getInstance(final Class<T> clazz, final boolean autoCommit) {
    return new Update<>(clazz, autoCommit);
  }

  public void setAutoCommit(final boolean autoCommit) {
    this.executor.getTransaction().setAutoCommit(autoCommit);
  }

  public void commit() {
    this.executor.getTransaction().commit();
  }

  public Update<T> where(@Nonnull final Condition<T, ?> condition) {
    this.condition = condition;
    return this;
  }

  /**
   * 创建.
   *
   * @throws DataAccessException 如果执行失败
   */
  public int create(@Nonnull final T t) throws DataAccessException {
    return create(t, null);
  }

  /**
   * 创建.
   *
   * @throws DataAccessException 如果执行失败
   */
  public int create(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    boolean returnPk = true;
    final Object existId = BeanUtils.getFieldValue(t, idField);
    if (existId == null) {
      final Object id = idGenerator.nextId(t);
      if (id != null) {
        returnPk = false;
        BeanUtils.setFieldValue(t, idField, id);
      }
    }
    setCurrentTime(t, entityInfo.getCreateTimeField());
    setCurrentTime(t, entityInfo.getUpdateTimeField());
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    final AtomicInteger index = new AtomicInteger(1);

    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getWritingFields(t, fields, SqlStatementType.INSERT)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              columns.append(columns.length() > 0 ? ",".concat(column) : column);
              valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
              getArgsValueSetConsumer(index, field, BeanUtils.getFieldValue(t, field), list);
            });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql = String.format(sql, entityInfo.getName(), columns, valueMarks);
    if (log.isDebugEnabled()) {
      log.debug("create:[{}]", execSql);
    }
    KeyHolder keyHolder = new GeneratedKeyHolder();
    final int update =
        executor.update(
            connection -> {
              final PreparedStatement ps =
                  DataSourceUtils.preparedStatement(
                      connection, execSql, Statement.RETURN_GENERATED_KEYS);
              list.forEach(val -> val.accept(ps));
              return ps;
            },
            keyHolder);
    // 写入数据库生成的主键
    if (returnPk) {
      // dialect.getKeyHolderKey()
      Object returnId = keyHolder.getKeys().get(entityInfo.getIdColumn().getName());
      if (returnId instanceof BigInteger) {
        returnId = ((BigInteger) returnId).longValue();
      }
      if (returnId != null) {
        BeanUtils.setFieldValue(t, idField, returnId);
      }
    }
    return update;
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
   * 批量创建,TODO WARN 填充主键 keyHolder.getKeyList().
   *
   * @param ts the ts
   * @param fields the fields
   * @return 保存成功数 optional
   */
  public Optional<Integer> createBatch(final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    AtomicBoolean returnPk = new AtomicBoolean(true);
    ts.forEach(
        t -> {
          final Object idValue = BeanUtils.getFieldValue(t, idField);
          if (idValue == null) {
            final Object id = idGenerator.nextId(t);
            if (id != null) {
              returnPk.set(false);
              BeanUtils.setFieldValue(t, idField, id);
            }
          }
          setCurrentTime(t, entityInfo.getCreateTimeField());
          setCurrentTime(t, entityInfo.getUpdateTimeField());
        });
    // 取第一条记录获取批量保存sql
    final T t = ts.get(0);
    final List<Field> savingFields = getWritingFields(t, fields, SqlStatementType.INSERT);
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
    final String execSql = String.format(sql, entityInfo.getName(), columns, valueMarks);
    KeyHolder keyHolder = new GeneratedKeyHolder();
    AtomicInteger createRow = new AtomicInteger();
    BatchExecUtil.batchExec(
        "create batch",
        ts,
        configuration.getBatchSize(),
        execList -> {
          final int[] batchUpdate =
              executor.batchUpdate(
                  connection ->
                      DataSourceUtils.preparedStatement(
                          connection, execSql, Statement.RETURN_GENERATED_KEYS),
                  new BatchPreparedStatementSetter() {
                    @Override
                    public void setValues(@Nonnull final PreparedStatement ps, final int i) {
                      final AtomicInteger index = new AtomicInteger(1);
                      final T obj = execList.get(i);
                      savingFields.forEach(
                          field ->
                              setArgsValue(index, field, BeanUtils.getFieldValue(obj, field), ps));
                    }

                    @Override
                    public int getBatchSize() {
                      return execList.size();
                    }
                  },
                  keyHolder);
          final List<Map<String, Object>> list = keyHolder.getKeyList();
          if (returnPk.get()) {
            final String idName = entityInfo.getIdColumn().getName();
            for (int i = 0; i < list.size(); i++) {
              final Map<String, Object> columnValueMap = list.get(i);
              Object idValue = columnValueMap.get(idName);
              if (idValue instanceof BigInteger) {
                idValue = ((BigInteger) idValue).longValue();
              }
              BeanUtils.setFieldValue(ts.get(i), idField, idValue);
            }
          }
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
   * @return 受影响记录数
   */
  public int update(@Nonnull final T t) throws DataAccessException {
    return update(t, null);
  }

  /**
   * 更新,默认根据id更新.
   *
   * @param fields 当排除不为空时,忽略全局是否写入空配置
   * @return 受影响记录数
   */
  public int update(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    setCurrentTime(t, entityInfo.getUpdateTimeField());
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger(1);
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getWritingFields(t, fields, SqlStatementType.UPDATE)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              setBuilder.append(String.format(setBuilder.length() > 0 ? ",%s=?" : "%s=?", column));
              getArgsValueSetConsumer(index, field, BeanUtils.getFieldValue(t, field), list);
            });
    // 没有条件时,默认根据id更新
    if (condition.isEmpty()) {
      this.condition.eq(idField, BeanUtils.getFieldValue(t, idField));
    }
    this.condition.appendLogicNotDelete();
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder, condition.getString());
    if (log.isDebugEnabled()) {
      log.debug("update:[{}]", execSql);
    }
    return executor.update(
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
  public Optional<Integer> updateBatch(@Nonnull final List<T> ts) {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新,根据id更新.
   *
   * @param fields 当排除不为空时,忽略全局是否写入空配置
   * @return the int
   */
  public Optional<Integer> updateBatch(@Nonnull final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 取第一条记录获取批量更新sql
    final T t = ts.get(0);
    // 保存更新时间
    setCurrentTime(t, entityInfo.getUpdateTimeField());

    final List<Field> savingFields = getWritingFields(t, fields, SqlStatementType.UPDATE);
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
        String.format(sql, entityInfo.getName(), setBuilder, condition.getString());

    AtomicInteger updateRow = new AtomicInteger();
    BatchExecUtil.batchExec(
        "update batch",
        ts,
        configuration.getBatchSize(),
        execList -> {
          final int[] batchUpdate =
              executor.batchUpdate(
                  execSql,
                  new BatchPreparedStatementSetter() {
                    @Override
                    public void setValues(@Nonnull final PreparedStatement ps, final int i) {
                      final AtomicInteger index = new AtomicInteger(1);
                      final T obj = execList.get(i);
                      setCurrentTime(obj, entityInfo.getUpdateTimeField());
                      savingFields.forEach(
                          field ->
                              setArgsValue(index, field, BeanUtils.getFieldValue(obj, field), ps));
                      LambdaCondition.of(clazz)
                          .eq(idField, BeanUtils.getFieldValue(obj, idField))
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
      throw new NotAllowedDataAccessException("Must contain delete condition");
    }
    this.condition.appendLogicNotDelete();
    // 如果不支持逻辑删除
    if (!entityInfo.getLogicDeleteProp().isEnable()) {
      final String delSql = "delete from %s where %s";
      return executor.update(
          String.format(delSql, entityInfo.getName(), condition.getString()),
          ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
    }
    // 执行逻辑删除
    final Field updateTimeField = entityInfo.getUpdateTimeField();
    // 包含更新时间
    if (updateTimeField != null) {
      final String updateString =
          String.format(
              "update %s set %s=%s, %s=? where %s",
              entityInfo.getName(),
              entityInfo.getLogicDeletePropColumn().getName(),
              entityInfo.getLogicDeleteProp().isDeleted(),
              entityInfo.getFieldColumnMap().get(updateTimeField).getName(),
              condition.getString());
      return executor.update(
          updateString,
          ps -> {
            JdbcUtils.setObject(ps, 1, getCurrentTimeValue(updateTimeField));
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
          updateString,
          ps -> condition.getValues(new AtomicInteger(1)).forEach(val -> val.accept(ps)));
    }
  }

  /**
   * 获取写入字段.{@code fields}为null时,根据配置判断是否写入null字段;当排除不为空时,忽略全局是否写入空配置
   *
   * @see Configuration#isWriteNullProp()
   */
  private List<Field> getWritingFields(
      final @Nonnull T t, final Fields<T> fields, final SqlStatementType sqlStatementType) {
    final List<Field> savingFields;
    if (fields == null) {
      savingFields =
          configuration.isWriteNullProp()
              ? EntityUtils.getAllColumnFields(t.getClass(), sqlStatementType)
              : EntityUtils.getNonNullColumnFields(t, sqlStatementType);
    } else {
      final Set<Field> includeFields = fields.getIncludeFields();
      final Set<Field> excludeFields = fields.getExcludeFields();
      if (includeFields.isEmpty()) {
        // 当排除不为空时,忽略是否写入空配置
        savingFields =
            (!excludeFields.isEmpty() || configuration.isWriteNullProp()
                    ? EntityUtils.getAllColumnFields(t.getClass(), sqlStatementType)
                    : EntityUtils.getNonNullColumnFields(t, sqlStatementType))
                .stream().filter(field -> !excludeFields.contains(field)).collect(toList());
      } else {
        savingFields =
            includeFields.stream()
                .filter(field -> !excludeFields.contains(field))
                .collect(toList());
      }
    }
    Object id = BeanUtils.getFieldValue(t, idField);
    // 自增id,需要去掉
    if (id == null) {
      savingFields.remove(idField);
    }
    return savingFields;
  }

  /** 设置为当前时间. */
  private void setCurrentTime(@Nonnull final T t, final Field field) {
    if (field == null) {
      return;
    }
    final Object val = BeanUtils.getFieldValue(t, field);
    final Class<?> type = field.getType();
    if (val == null) {
      BeanUtils.setFieldValue(t, field, getCurrentTimeValue(field));
      return;
    }
    // 如果是基本类型int,long,默认值为0,此时也需要赋值
    if ((type.equals(int.class) || type.equals(long.class)) && (long) val == 0) {
      BeanUtils.setFieldValue(t, field, getCurrentTimeValue(field));
    }
  }

  /** 获取当前时间特定类型值,如果是Long/long型,使用毫秒;如果是Integer/int型,使用秒. */
  private Object getCurrentTimeValue(final Field field) {
    final Class<?> fieldType = field.getType();
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
      return new Date();
    }
    if (Integer.class.isAssignableFrom(fieldType) || int.class.equals(fieldType)) {
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
                  "params:[index:{},originValue:{},value:{}]", index.get(), originValue, value);
            }
            ps.setObject(index.getAndIncrement(), value);
          } catch (SQLException e) {
            throw new CommonException(e);
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
    if (log.isDebugEnabled()) {
      log.debug("params:[index:{},originValue:{},value:{}]", index.get(), originValue, value);
    }
    try {
      ps.setObject(index.getAndIncrement(), value);
    } catch (SQLException e) {
      log.warn(e.getMessage());
      log.error(e.getMessage(), e);
      throw new CommonException(e.getMessage(), e);
    }
  }
}
