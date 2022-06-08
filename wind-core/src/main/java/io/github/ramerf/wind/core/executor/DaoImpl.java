package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.condition.*;
import io.github.ramerf.wind.core.condition.function.AggregateSqlFunction;
import io.github.ramerf.wind.core.config.Configuration;
import io.github.ramerf.wind.core.config.Configuration.TimestampStrategy;
import io.github.ramerf.wind.core.domain.Page;
import io.github.ramerf.wind.core.domain.Pageable;
import io.github.ramerf.wind.core.exception.NotAllowedDataAccessException;
import io.github.ramerf.wind.core.exception.WindException;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.handler.ResultHandler;
import io.github.ramerf.wind.core.handler.ResultHandlerUtil;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.metadata.DbMetaData;
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
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.sql.DataSource;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.GROUP_BY;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.WHERE;
import static java.util.stream.Collectors.toList;

/**
 * @author ramer
 * @since 2022.03.12
 */
@Slf4j
@SuppressWarnings("DuplicatedCode")
public class DaoImpl implements Dao {
  private final Executor executor;
  private final Configuration configuration;
  private final boolean autoCommit;
  /** 包含写入操作. */
  private boolean write;

  public DaoImpl(
      final Configuration configuration, final Executor executor, final boolean autoCommit) {
    this.configuration = configuration;
    this.executor = executor;
    this.autoCommit = autoCommit;
  }

  /*查询方法.*/

  /**
   * count查询.
   *
   * @return long long
   */
  @Override
  public long fetchCount(@Nonnull final Condition<?, ?> condition) {
    final String conditionClause = getConditionClause(condition);
    final String sql =
        conditionClause.contains(GROUP_BY.operator())
            ? "select sum(b.a) from (select 1 a from %s%s) b"
            : "select count(1) from %s%s";
    final Class<?> clazz = condition.getClazz();
    //noinspection unchecked
    return this.executor.fetchCount(
        new SqlParam<>()
            .setSql(String.format(sql, getTableName(clazz), conditionClause))
            .setClazz(Long.class)
            .setEntityClazz((Class<Object>) clazz)
            .setAggregateFunction(AggregateSqlFunction.COUNT)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)));
  }

  /** 自定义sql查询count. */
  @Override
  public long fetchCount(final String sql, final Object... args) {
    return Optional.ofNullable(
            executor.<Object, Long>queryForObject(
                new SqlParam<>()
                    .setClazz(Long.class)
                    .setSql(sql)
                    .setAggregateFunction(AggregateSqlFunction.COUNT),
                args))
        .orElseGet(() -> (Long) BeanUtils.getPrimitiveDefaultValue(long.class));
  }

  /** 查询单条记录. */
  @Override
  public <T> T fetchOne(@Nonnull final Condition<T, ?> condition) {
    return fetchOne(condition, null, condition.getClazz());
  }

  /** 查询单条记录. */
  @Override
  public <T> T fetchOne(@Nonnull final Condition<T, ?> condition, final Fields<T> fields) {
    return fetchOne(condition, fields, condition.getClazz());
  }

  @Override
  public <T, R> R fetchOne(@Nonnull final Condition<T, ?> condition, final Class<R> respClazz) {
    return fetchOne(condition, null, respClazz);
  }

  /** 查询单条记录. */
  @Override
  public <T, R> R fetchOne(
      @Nonnull final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz) {
    return fetchOne(condition, fields, respClazz, null);
  }

  /** 查询单条记录. */
  @Override
  public <T, R> R fetchOne(
      @Nonnull final Condition<T, ?> condition,
      final Fields<T> fields,
      final Class<R> respClazz,
      final ResultHandler<R> resultHandler) {
    final String orderByClause = getOrderByClause(condition.getPageRequest());
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(condition.getPageRequest());
    final Class<T> clazz = condition.getClazz();
    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields, clazz),
            getTableName(clazz),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchOne:[{}]", sql);
    }
    return executor.fetchOne(
        new SqlParam<T>()
            .setSql(sql)
            .setEntityClazz(clazz)
            .setClazz(respClazz == null ? clazz : respClazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        resultHandler);
  }

  /** 自定义sql查询单个. */
  @Override
  public <R> R fetchOne(final String sql, final Class<R> respClazz, final Object... args) {
    List<R> rs = fetchAll(sql, respClazz, args);
    return rs.isEmpty() ? null : rs.get(0);
  }

  /** 查询列表. */
  @Override
  public <T> List<T> fetchAll(@Nonnull final Condition<T, ?> condition) {
    return fetchAll(condition, null, condition.getClazz());
  }

  /** 查询列表. */
  @Override
  public <T> List<T> fetchAll(@Nonnull final Condition<T, ?> condition, final Fields<T> fields) {
    return fetchAll(condition, fields, null);
  }

  @Override
  public <T, R> List<R> fetchAll(
      @Nonnull final Condition<T, ?> condition, final Class<R> respClazz) {
    return fetchAll(condition, null, respClazz);
  }

  /** 查询列表,查询指定页,返回指定对象. */
  @Override
  public <T, R> List<R> fetchAll(
      @Nonnull final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz) {
    final Pageable pageable = condition.getPageRequest();
    final String orderByClause = getOrderByClause(pageable);
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(pageable);

    final Class<T> clazz = condition.getClazz();
    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields, clazz),
            getTableName(clazz),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchAll:[{}]", sql);
    }
    return executor.fetchAll(
        new SqlParam<T>()
            .setSql(sql)
            .setEntityClazz(clazz)
            .setClazz(respClazz == null ? clazz : respClazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        respClazz);
  }

  /** 自定义sql查询列表. */
  @Override
  public <T, R> List<R> fetchAll(final String sql, final Class<R> respClazz, final Object... args) {
    return executor.query(
        new SqlParam<T>().setSql(sql).setClazz(respClazz),
        ps -> {
          for (int i = 1; i < args.length + 1; i++) {
            JdbcUtils.setObject(ps, i, args[i - 1]);
          }
        },
        ResultHandlerUtil.getResultHandler(respClazz));
  }

  @Override
  public <T> Page<T> fetchPage(final Condition<T, ?> condition) throws DataAccessException {
    return fetchPage(condition, null, condition.getClazz());
  }

  @Override
  public <T> Page<T> fetchPage(final Condition<T, ?> condition, final Fields<T> fields)
      throws DataAccessException {
    return fetchPage(condition, fields, condition.getClazz());
  }

  @Override
  public <T, R> Page<R> fetchPage(final Condition<T, ?> condition, final Class<R> respClazz)
      throws DataAccessException {
    return fetchPage(condition, null, respClazz);
  }

  /** 分页查询. */
  @Override
  public <T, R> Page<R> fetchPage(
      final Condition<T, ?> condition, final Fields<T> fields, final Class<R> respClazz) {
    final Pageable pageable = condition.getPageRequest();
    final String orderByClause = getOrderByClause(pageable);
    final String conditionClause = getConditionClause(condition);
    final String limitClause = getLimitClause(pageable);

    final Class<T> clazz = condition.getClazz();
    final String templateSql = "select %s from %s%s";
    final String sql =
        String.format(
            templateSql,
            getQueryString(fields, clazz),
            getTableName(clazz),
            conditionClause.concat(orderByClause).concat(limitClause));
    if (log.isTraceEnabled()) {
      log.debug("fetchPage:[{}]", sql);
    }
    return this.executor.fetchPage(
        new SqlParam<T>()
            .setSql(sql)
            .setEntityClazz(clazz)
            .setClazz(respClazz == null ? clazz : respClazz)
            .setCondition(condition)
            .setStartIndex(new AtomicInteger(1)),
        pageable,
        fetchCount(condition));
  }

  /** 获取查询列.{@code fields}为null时,根据配置判断是否写入null字段;当排除不为空时,忽略全局是否写入空配置 */
  private String getQueryString(final Fields<?> fields, final Class<?> clazz) {
    if (fields == null)
      return EntityUtils.getAllColumnFields(clazz, null).stream()
          .map(EntityUtils::fieldToColumn)
          .collect(Collectors.joining(","));
    final Collection<Field> queryFields;
    final Set<Field> includeFields = fields.getIncludeFields();
    final Set<Field> excludeFields = fields.getExcludeFields();
    if (!includeFields.isEmpty()) {
      queryFields =
          excludeFields.isEmpty()
              ? includeFields
              : includeFields.stream()
                  .filter(include -> !excludeFields.contains(include))
                  .collect(Collectors.toSet());
    } else {
      final List<Field> allColumnFields = EntityUtils.getAllColumnFields(fields.getClazz(), null);
      // 当排除不为空时,忽略是否写入空配置
      queryFields =
          excludeFields.isEmpty()
              ? allColumnFields
              : allColumnFields.stream()
                  .filter(include -> !excludeFields.contains(include))
                  .collect(Collectors.toSet());
    }
    return queryFields.stream().map(EntityUtils::fieldToColumn).collect(Collectors.joining(","));
  }

  private String getTableName(@NonNull final Class<?> clazz) {
    EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
    return entityInfo.getName();
  }

  private String getConditionClause(Condition<?, ?> condition) {
    condition = condition.appendLogicNotDelete();
    return condition.isEmpty() ? "" : WHERE.operator().concat(condition.getString());
  }

  private String getOrderByClause(final Pageable pageRequest) {
    Pageable pageable = pageRequest == null ? Pageable.unpaged() : pageRequest;
    if (pageable == null) return "";
    String orderBy =
        pageable.getSort().stream()
            .map(s -> s.getProperty().concat(" ").concat(s.getDirection().name()))
            .collect(Collectors.joining(","));
    return orderBy.isEmpty() ? "" : " order by " + orderBy;
  }

  private String getLimitClause(final Pageable pageRequest) {
    Pageable pageable = pageRequest == null ? Pageable.unpaged() : pageRequest;
    return pageable.isPaged()
        ? String.format(" limit %s offset %s", pageable.getPageSize(), pageable.getOffset())
        : "";
  }

  /*查询方法.*/

  /* 写入方法. */
  /**
   * 创建.
   *
   * @throws DataAccessException 如果执行失败
   */
  @Override
  public int create(@Nonnull final Object t) throws DataAccessException {
    return create(t, null);
  }

  /**
   * 创建.
   *
   * @throws DataAccessException 如果执行失败
   */
  @Override
  public <T> int create(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    boolean returnPk = true;
    final Class<?> clazz = t.getClass();
    final EntityInfo entityInfo = getEntityInfo(clazz);
    final Field idField = entityInfo.getIdColumn().getField();
    final IdGenerator idGenerator = entityInfo.getIdGenerator();
    final Object existId = BeanUtils.getFieldValue(t, idField);
    if (existId == null) {
      final Object id = idGenerator.nextId(t);
      if (id != null) {
        returnPk = false;
        BeanUtils.setFieldValue(t, idField, id);
      }
    }
    setCurrentTime(t, entityInfo.getCreateTimeField(), false);
    setCurrentTime(
        t,
        entityInfo.getUpdateTimeField(),
        configuration.getUpdateTimeStrategy().equals(TimestampStrategy.ALWAYS));
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
    if (log.isTraceEnabled()) {
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
      Object returnId = keyHolder.getKeys().get(getKeyHolderKey(clazz));
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
  @Override
  public Optional<Integer> createBatch(final List<?> ts) {
    return createBatch(ts, null);
  }

  /**
   * 批量创建,TODO WARN 填充主键 keyHolder.getKeyList().
   *
   * @param ts the ts
   * @param fields the fields
   * @return 保存成功数 optional
   */
  @Override
  public <T> Optional<Integer> createBatch(final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    final Class<?> clazz = ts.get(0).getClass();
    final EntityInfo entityInfo = getEntityInfo(clazz);
    final Field idField = entityInfo.getIdColumn().getField();
    final IdGenerator idGenerator = entityInfo.getIdGenerator();
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
          setCurrentTime(t, entityInfo.getCreateTimeField(), false);
          setCurrentTime(
              t,
              entityInfo.getUpdateTimeField(),
              configuration.getUpdateTimeStrategy().equals(TimestampStrategy.ALWAYS));
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
            final String keyHolderKey = getKeyHolderKey(clazz);
            for (int i = 0; i < list.size(); i++) {
              final Map<String, Object> columnValueMap = list.get(i);
              Object idValue = columnValueMap.get(keyHolderKey);
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

  private String getKeyHolderKey(final Class<?> clazz) {
    final DataSource dataSource = configuration.getJdbcEnvironment().getDataSource();
    final DbMetaData dbMetaData = DbMetaData.getInstance(dataSource, configuration.getDialect());
    final String keyHolderKey = dbMetaData.getDialect().getKeyHolderKey();
    return keyHolderKey != null ? keyHolderKey : getEntityInfo(clazz).getIdColumn().getName();
  }

  /**
   * 更新,默认根据id更新且不更新值为null的列.
   *
   * @param t the t
   * @return 受影响记录数
   */
  @Override
  public int update(@Nonnull final Object t) throws DataAccessException {
    return update(t, null);
  }

  /**
   * 更新,默认根据id更新且不更新值为null的列.
   *
   * @param t the t
   * @return 受影响记录数
   */
  @Override
  public <T> int update(@Nonnull final T t, final Fields<T> fields) throws DataAccessException {
    //noinspection unchecked
    return update(t, fields, Cnd.of((Class<T>) t.getClass()));
  }

  /**
   * 更新,默认根据id更新.
   *
   * @param fields 当排除不为空时,忽略全局是否写入空配置
   * @return 受影响记录数
   */
  @Override
  public <T> int update(
      @Nonnull final T t,
      @Nullable final Fields<T> fields,
      @Nonnull final Condition<T, ?> condition)
      throws DataAccessException {
    final EntityInfo entityInfo = getEntityInfo(t.getClass());
    final Field idField = entityInfo.getIdColumn().getField();

    setCurrentTime(
        t,
        entityInfo.getUpdateTimeField(),
        configuration.getUpdateTimeStrategy().equals(TimestampStrategy.ALWAYS));
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
      condition.eq(idField, BeanUtils.getFieldValue(t, idField));
    }
    condition.appendLogicNotDelete();
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, entityInfo.getName(), setBuilder, condition.getString());
    if (log.isTraceEnabled()) {
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
  @Override
  public Optional<Integer> updateBatch(@Nonnull final List<?> ts) {
    return updateBatch(ts, null);
  }

  /**
   * 批量更新,根据id更新.
   *
   * @param fields 当排除不为空时,忽略全局是否写入空配置
   * @return the int
   */
  @Override
  public <T> Optional<Integer> updateBatch(@Nonnull final List<T> ts, final Fields<T> fields) {
    if (CollectionUtils.isEmpty(ts)) {
      return Optional.empty();
    }
    // 取第一条记录获取批量更新sql
    final T t = ts.get(0);
    @SuppressWarnings("unchecked")
    final Class<T> clazz = (Class<T>) t.getClass();
    final EntityInfo entityInfo = getEntityInfo(clazz);
    final Field idField = entityInfo.getIdColumn().getField();
    // 保存更新时间
    setCurrentTime(
        t,
        entityInfo.getUpdateTimeField(),
        configuration.getUpdateTimeStrategy().equals(TimestampStrategy.ALWAYS));

    final List<Field> savingFields = getWritingFields(t, fields, SqlStatementType.UPDATE);
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    savingFields.forEach(
        field ->
            setBuilder.append(
                String.format(
                    setBuilder.length() > 0 ? ",%s=?" : "%s=?", EntityUtils.fieldToColumn(field))));
    Cnd<T> condition = Cnd.of(clazz);
    // 保证占位符对应
    condition.eq(idField, null);
    condition.appendLogicNotDelete();
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
                      setCurrentTime(
                          obj,
                          entityInfo.getUpdateTimeField(),
                          configuration.getUpdateTimeStrategy().equals(TimestampStrategy.ALWAYS));
                      savingFields.forEach(
                          field ->
                              setArgsValue(index, field, BeanUtils.getFieldValue(obj, field), ps));
                      Cnd.of(clazz)
                          .eq(idField, BeanUtils.getFieldValue(obj, idField))
                          .appendLogicNotDelete()
                          .getValues(index)
                          .forEach(val -> val.accept(ps));
                    }

                    @Override
                    public int getBatchSize() {
                      return execList.size();
                    }
                  });
          updateRow.getAndAdd(Arrays.stream(batchUpdate).filter(o -> o > 0).map(o -> 1).sum());
        });
    return updateRow.get() == ts.size() ? Optional.empty() : Optional.of(updateRow.get());
  }

  @Override
  public int update(final String sql, @Nonnull final PreparedStatementSetter pss)
      throws DataAccessException {
    return executor.update(sql, pss);
  }

  /**
   * 条件删除,不允许条件为空.
   *
   * @return 删除记录数 int
   * @throws DataAccessException 如果执行失败
   */
  @Override
  public int delete(@Nonnull final Condition<?, ?> condition) throws DataAccessException {
    // 不包含删除条件,抛异常
    if (condition.isEmpty()) {
      throw new NotAllowedDataAccessException("Must contain delete condition");
    }
    condition.appendLogicNotDelete();
    final EntityInfo entityInfo = getEntityInfo(condition.getClazz());
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
  private <T> List<Field> getWritingFields(
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
    final Field idField = getEntityInfo(t.getClass()).getIdColumn().getField();
    Object id = BeanUtils.getFieldValue(t, idField);
    // 自增id,需要去掉
    if (id == null) {
      savingFields.remove(idField);
    }
    return savingFields;
  }

  /**
   * 设置为当前时间.
   *
   * @param force 当为false且字段不为空时不执行任何操作,否则总是设置为当前时间
   */
  private void setCurrentTime(@Nonnull final Object t, final Field field, final boolean force) {
    if (field == null) {
      return;
    }
    if (force) {
      BeanUtils.setFieldValue(t, field, getCurrentTimeValue(field));
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
            if (log.isTraceEnabled()) {
              log.debug(
                  "params:[index:{},originValue:{},value:{}]", index.get(), originValue, value);
            }
            ps.setObject(index.getAndIncrement(), value);
          } catch (SQLException e) {
            throw new WindException(e);
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
      log.debug("params:[index:{},originValue:{},value:{}]", index.get(), originValue, value);
    }
    try {
      ps.setObject(index.getAndIncrement(), value);
    } catch (SQLException e) {
      throw new WindException(e);
    }
  }

  private EntityInfo getEntityInfo(final Class<?> clazz) {
    return EntityHelper.getEntityInfo(clazz);
  }

  /* 写入方法. */

  @Override
  public Configuration getConfiguration() {
    return configuration;
  }

  @Override
  public void commit() {
    commit(false);
  }

  @Override
  public void commit(boolean force) {
    executor.commit(isCommitOrRollbackRequired(force));
    write = false;
  }

  @Override
  public void rollback() {
    rollback(false);
  }

  @Override
  public void rollback(boolean force) {
    executor.rollback(isCommitOrRollbackRequired(force));
    write = false;
  }

  @Override
  public Connection getConnection() {
    return executor.getTransaction().getConnection();
  }

  private boolean isCommitOrRollbackRequired(boolean force) {
    return (!autoCommit && write) || force;
  }
}
