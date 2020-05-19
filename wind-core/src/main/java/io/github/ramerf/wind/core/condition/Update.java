package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.request.AbstractEntityRequest;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.TypeConverterHelper;
import io.github.ramerf.wind.core.helper.TypeConverterHelper.ValueType;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import static java.util.stream.Collectors.toList;

/**
 * The type Update.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/13
 */
@Slf4j
@Component
@SuppressWarnings("unused")
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class Update {
  private Condition<?> condition;
  private String tableName;

  @SuppressWarnings("FieldCanBeLocal")
  private String logicDeleteField;

  @SuppressWarnings("FieldCanBeLocal")
  private boolean logicDeleted;

  private static JdbcTemplate JDBC_TEMPLATE;

  /** Instantiates a new Update. */
  Update() {
    Update.JDBC_TEMPLATE = AppContextInject.getBean(JdbcTemplate.class);
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
    final WindConfiguration configuration = AppContextInject.getBean(WindConfiguration.class);
    logicDeleteField = configuration.getLogicDeleteField();
    logicDeleted = configuration.isLogicDeleted();
    if (Objects.isNull(clazz) && StringUtils.isEmpty(tableName)) {
      throw CommonException.of("[clazz,tableName]不能同时为空");
    }
    this.tableName = StringUtils.nonEmpty(tableName) ? tableName : EntityUtils.getTableName(clazz);
    this.condition = QueryColumnFactory.getInstance(clazz).getCondition();
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
   * Create int.
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
    t.setId(AppContextInject.getBean(SnowflakeIdWorker.class).nextId());
    final Date now = new Date();
    if (Objects.isNull(t.getCreateTime())) {
      t.setCreateTime(now);
    }
    if (Objects.isNull(t.getUpdateTime())) {
      t.setUpdateTime(now);
    }
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, includeNullProps)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              columns.append(columns.length() > 0 ? ",".concat(column) : column);
              valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
              setParameterConsumer(index, field, BeanUtils.invoke(t, field, null), list);
            });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql = String.format(sql, tableName, columns.toString(), valueMarks.toString());
    if (JDBC_TEMPLATE.update(execSql, ps -> list.forEach(val -> val.accept(ps))) != 1) {
      throw CommonException.of(ResultCode.API_FAIL_EXEC_CREATE);
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
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntityPoJo> int createBatch(
      final List<T> ts, @Nonnull final IFunction<T, ?>... includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    final AtomicInteger count = new AtomicInteger(0);
    ts.forEach(
        t -> {
          try {
            create(ts.get(0), includeNullProps);
            count.incrementAndGet();
          } catch (Exception e) {
            log.warn(e.getMessage());
            log.error(e.getMessage(), e);
          }
        });
    return count.get();
  }

  @SuppressWarnings("unchecked")
  private <T extends AbstractEntity> Set<Field> getSavingFields(
      final T t, final IFunction<T, ?>... includeNullProps) {
    final Set<Field> savingFields = new HashSet<>(EntityUtils.getNonNullColumnFields(t));
    if (Objects.nonNull(includeNullProps) && includeNullProps.length > 0) {
      Arrays.stream(includeNullProps).forEach(prop -> savingFields.add(prop.getField()));
    }
    return savingFields;
  }

  /**
   * 保存所有字段,包含null.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param includeNullProps 即使值为null也保存的属性
   * @return the long
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntityPoJo> int update(
      @Nonnull final T t, final IFunction<T, ?>... includeNullProps) {
    t.setUpdateTime(new Date());
    final StringBuilder setBuilder = new StringBuilder();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    getSavingFields(t, includeNullProps)
        .forEach(
            field -> {
              final String column = EntityUtils.fieldToColumn(field);
              setBuilder.append(String.format(setBuilder.length() > 0 ? ",%s=?" : "%s=?", column));
              setParameterConsumer(index, field, BeanUtils.invoke(t, field, null), list);
            });
    final List<Function<PreparedStatement, Object>> values = condition.getValues();
    // TODO-WARN 这里有bug,值没有包含下面的条件
    // 没有更新条件时,根据id更新
    if (values.size() <= 1) {
      where(cond -> cond.eq(AbstractEntityPoJo::setId, t.getId()));
    }
    values.forEach(
        value ->
            list.add(
                ps -> {
                  try {
                    ps.setObject(index.incrementAndGet(), value.apply(ps));
                  } catch (SQLException e) {
                    throw CommonException.of(e);
                  }
                }));
    final String sql = "UPDATE %s SET %s WHERE %s";
    final String execSql =
        String.format(sql, tableName, setBuilder.toString(), condition.getString());
    return JDBC_TEMPLATE.update(execSql, ps -> list.forEach(val -> val.accept(ps)));
  }

  /**
   * Update batch int.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNullProps 即使值为null也保存的属性
   * @return the int
   */
  @SuppressWarnings("unchecked")
  public <T extends AbstractEntityPoJo> int updateBatch(
      @Nonnull final List<T> ts, final IFunction<T, ?>... includeNullProps) {
    if (CollectionUtils.isEmpty(ts)) {
      return 0;
    }
    AtomicInteger count = new AtomicInteger(0);
    ts.forEach(
        t -> {
          try {
            update(t, includeNullProps);
            count.getAndIncrement();
          } catch (Exception e) {
            log.warn(e.getMessage());
            log.error(e.getMessage(), e);
          }
        });
    return count.get();
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
    final List<Function<PreparedStatement, Object>> values = condition.getValues();
    // 仅包含逻辑未删除条件
    if (values.size() <= 1) {
      throw CommonException.of(ResultCode.API_FAIL_DELETE_NO_CONDITION);
    }
    final String sql = "update %s set %s=%s where %s";
    final String updateString =
        String.format(
            sql,
            tableName,
            StringUtils.camelToUnderline(logicDeleteField),
            logicDeleted,
            condition.getString());
    return JDBC_TEMPLATE.update(
        updateString,
        ps ->
            values.stream().map(value -> value.apply(ps)).collect(toList()).toArray(new Object[0]));
  }

  private void setParameterConsumer(
      AtomicInteger index, Field field, Object val, List<Consumer<PreparedStatement>> list) {
    final Object originValue = val;
    Consumer<PreparedStatement> function =
        ps -> {
          try {
            final Object value =
                TypeConverterHelper.toJdbcValue(ValueType.of(originValue, field), ps);
            if (log.isDebugEnabled()) {
              log.info(
                  "setParameterConsumer:[index:{},originValue:{},value:{}]",
                  index.get(),
                  originValue,
                  value);
            }
            ps.setObject(index.incrementAndGet(), value);
          } catch (SQLException e) {
            throw CommonException.of(e);
          }
        };
    list.add(function);
  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static Update getInstance() {
    return AppContextInject.getBean(Update.class);
  }
}
