package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.entity.response.ResultCode;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.factory.QueryColumnFactory;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.helper.TypeConverterHelper;
import io.github.ramerf.wind.core.repository.AbstractBaseRepository;
import io.github.ramerf.wind.core.util.*;
import java.lang.reflect.Field;
import java.sql.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import javax.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * The type Update.
 *
 * @author Tang Xiaofeng
 * @since 2020 /1/13
 */
@Slf4j
@Component
@Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class Update {
  @Resource private AbstractBaseRepository repository;
  private Condition<?> condition;
  private String tableName;

  @SuppressWarnings("FieldCanBeLocal")
  private String logicDeleteField;

  @SuppressWarnings("FieldCanBeLocal")
  private boolean logicDeleted;

  private static JdbcTemplate JDBC_TEMPLATE;

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

  public <T extends AbstractEntity> int create(final T t) {
    final List<Field> fields = EntityUtils.getNonNullColumnFields(t);
    // 插入列
    final StringBuilder columns = new StringBuilder();
    // values中的?占位符
    final StringBuilder valueMarks = new StringBuilder();
    List<Object> values = new LinkedList<>();
    final AtomicInteger index = new AtomicInteger();
    List<Consumer<PreparedStatement>> list = new LinkedList<>();
    fields.forEach(
        field -> {
          final String column = EntityUtils.fieldToColumn(field);
          columns.append(columns.length() > 0 ? ",".concat(column) : column);
          valueMarks.append(valueMarks.length() > 0 ? ",?" : "?");
          final Object originValue = BeanUtils.invoke(t, field, null);
          Consumer<PreparedStatement> function =
              ps -> {
                try {
                  final Object value =
                      TypeConverterHelper.toJdbcValue(originValue, field.getGenericType(), ps);
                  if (originValue instanceof Collection) {
                    ps.setArray(index.incrementAndGet(), (Array) value);
                  } else {
                    ps.setObject(index.incrementAndGet(), value);
                  }
                } catch (SQLException throwables) {
                  throw CommonException.of(throwables);
                }
              };
          list.add(function);
        });
    final String sql = "INSERT INTO %s(%s) VALUES(%s)";
    final String execSql = String.format(sql, tableName, columns.toString(), valueMarks.toString());
    if (log.isDebugEnabled()) {
      SqlHelper.printSqlWithVal(execSql, values);
    }
    return JDBC_TEMPLATE.update(execSql, ps -> list.forEach(val -> val.accept(ps)));
  }

  /**
   * 条件删除,不允许条件为空.
   *
   * @return 删除记录数
   */
  public int delete() {
    final List<Object> values = condition.getValues();
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
    if (log.isDebugEnabled()) {
      SqlHelper.printSqlWithVal(updateString, values);
    }
    return JDBC_TEMPLATE.update(updateString, values.toArray(new Object[0]));
  }

  /**
   * 保存,值不为null的字段.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @return the long
   */
  public <T extends AbstractEntity> long createBatch(List<T> ts) {
    //    return createBatch(ts, false);
    return 0;
  }

  /**
   * 保存所有字段,包含null.
   *
   * @param <T> the type parameter
   * @param ts the ts
   * @param includeNull the include null
   * @return the long
   */
  @SuppressWarnings({"rawtypes", "unchecked"})
  //  public <T extends AbstractEntity> long createBatch(List<T> ts, final boolean includeNull) {
  //    if (CollectionUtils.isEmpty(ts)) {
  //      return 0;
  //    }
  //    final T t = ts.get(0);
  //    // 待插入的列
  //    List<String> rowColumns = new LinkedList<>();
  //    List<PropertyDescriptor> propertyDescriptors = new LinkedList<>();
  //    final BeanWrapper wrapper = new BeanWrapperImpl(t);
  //    Stream<PropertyDescriptor> stream =
  //        Stream.of(wrapper.getPropertyDescriptors())
  //            .filter(o -> !Objects.equals(o.getName(), "class"));
  //    if (!includeNull) {
  //      stream = stream.filter(o -> Objects.nonNull(wrapper.getPropertyValue(o.getName())));
  //    }
  //    stream.forEach(
  //        o -> {
  //          rowColumns.add(StringUtils.camelToUnderline(o.getName()));
  //          propertyDescriptors.add(o);
  //        });
  //    final List<InsertValue> insertValues =
  //        ts.stream()
  //            .map(
  //                o ->
  //                    new InsertValue(
  //                        propertyDescriptors.stream()
  //                            .map(
  //                                prop -> {
  //                                  // 值转换,如果加了转换器,这里需要开分支.
  //                                  final Object value =
  //                                      new BeanWrapperImpl(o).getPropertyValue(prop.getName());
  //                                  final TypeConverter typeConverter =
  //
  // AppContextInject.getBean(TypeConverterRegistryFactory.class)
  //                                          .getToJdbcTypeConverter(
  //                                              value,
  // prop.getReadMethod().getGenericReturnType());
  //                                  if (Objects.nonNull(typeConverter)) {
  //                                    return typeConverter.convertToJdbc(value);
  //                                  }
  //                                  return value;
  //                                })
  //                            .collect(Collectors.toList())))
  //            .collect(Collectors.toList());
  //    final InsertValue insertValue =
  //
  // InsertStatement.getInstance().insert(t.getClass()).columns(rowColumns).values(insertValues);
  //    return repository.insertSelect(
  //        insertValue.getInsertStr().concat(insertValue.getColumnStr()),
  // insertValue.getValuesStr());
  //  }

  /**
   * 保存所有字段,包含null.
   *
   * @param <T> the type parameter
   * @param t the t
   * @param includeNull 是否包含为空的字段
   * @return the long
   */
  //  public <T extends AbstractEntityPoJo> int update(@Nonnull final T t, final boolean
  // includeNull) {
  //    if (includeNull) {
  //      log.warn("updateBatch:更新包含Null[{}]", t);
  //    }
  //    t.setUpdateTime(new Date());
  //    List<Object> values = new LinkedList<>();
  //    final StringBuilder setBuilder = new StringBuilder();
  //    final List<Field> fields =
  //        includeNull
  //            ? EntityUtils.getAllColumnFields(t.getClass())
  //            : EntityUtils.getNonNullColumnFields(t);
  //    if (log.isDebugEnabled()) {
  //      log.debug("updateBatch:[{}]", fields);
  //    }
  //    // 过滤非数据库字段,当前使用的是mybatis-plus的注解,后期考虑切换自定义注解
  //    fields.stream()
  //        .filter(
  //            field ->
  //                Optional.ofNullable(field.getAnnotation(TableField.class))
  //                    .map(TableField::exist)
  //                    .orElse(true))
  //        .forEach(
  //            field -> {
  //              final String column = EntityUtils.fieldToColumn(field);
  //              setBuilder.append(String.format(setBuilder.length() > 0 ? ",%s=?" : "%s=?",
  // column));
  //              final Object value =
  //                  TypeConverterHelper.toJdbcValue(
  //                      BeanUtils.invoke(t, field, null), field.getGenericType());
  //              log.debug("updateBatch:[field:{},value:{}]", field.getName(), value);
  //              values.add(value);
  //            });
  //    final String conditionString = condition.getString();
  //    values.addAll(condition.getValues());
  //    final String sql = "UPDATE %s SET %s WHERE %s";
  //    final String execSql = String.format(sql, tableName, setBuilder.toString(),
  // conditionString);
  //    if (log.isDebugEnabled()) {
  //      SqlHelper.printSqlWithVal(execSql, values);
  //    }
  //    return JDBC_TEMPLATE.update(execSql, values.toArray(new Object[0]));
  //  }

  /**
   * Gets instance.
   *
   * @return the instance
   */
  public static Update getInstance() {
    return AppContextInject.getBean(Update.class);
  }
}
