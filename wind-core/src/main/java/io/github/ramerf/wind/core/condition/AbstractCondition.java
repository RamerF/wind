package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.*;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.EntityUtils;
import io.github.ramerf.wind.core.util.StringUtils;
import java.lang.reflect.Field;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.Getter;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;
import static java.util.stream.Collectors.toCollection;

/**
 * 条件构造.
 *
 * @since 2020.09.16
 * @author ramer
 */
@Slf4j
@ToString
public abstract class AbstractCondition<T> extends AbstractQueryEntity<T> implements Condition<T> {
  /** where后的字符串,参数占位符为 ?. */
  protected final List<String> conditionSql = new LinkedList<>();

  /** 占位符对应的值. */
  @Getter protected final List<ValueType> valueTypes = new LinkedList<>();

  private static final char[] alphabets =
      new char[] {
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
      };

  protected AbstractCondition() {}

  public AbstractCondition(final QueryColumn<T> queryColumn) {
    setEntityInfo(queryColumn.getEntityInfo());
    setQueryEntityMetaData(queryColumn.getQueryEntityMetaData());
  }

  public AbstractCondition(final Class<T> clazz) {
    this(clazz, null, null);
  }

  public AbstractCondition(final Class<T> clazz, String tableName, String tableAlia) {
    if (clazz == null && tableName == null && tableAlia == null) {
      throw CommonException.of("[clazz,tableName,tableAlia]不能同时为空");
    }
    final WindConfiguration configuration = AppContextInject.getBean(WindConfiguration.class);
    if (clazz != null) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
      // 如果tableName不为空,需要覆盖entityInfo的值.传入的tableName优先级最高,因为支持使用不相关的类查询表
      if (tableName != null) {
        entityInfo.setName(tableName);
      } else {
        tableName = entityInfo.getName();
      }
      setEntityInfo(entityInfo);
    }
    final QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();
    queryEntityMetaData.setClazz(clazz);
    queryEntityMetaData.setTableName(tableName);
    tableAlia = tableAlia == null ? tableName : tableAlia;
    queryEntityMetaData.setTableAlia(tableAlia);
    String fromTable = tableName;
    if (tableAlia != null && !tableAlia.equals(tableName)) {
      fromTable = tableName.concat(" ").concat(tableAlia);
    }
    queryEntityMetaData.setFromTable(fromTable);
    setQueryEntityMetaData(queryEntityMetaData);
  }

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  public Condition<T> and(final String sql) {
    if (StringUtils.nonEmpty(sql)) {
      conditionSql.add((conditionSql.size() > 0 ? AND.operator : "").concat(sql));
    }
    return this;
  }

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  public Condition<T> or(final String sql) {
    if (StringUtils.nonEmpty(sql)) {
      conditionSql.add((conditionSql.size() > 0 ? OR.operator : "").concat(sql));
    }
    return this;
  }

  public AbstractCondition<T> condition(final boolean genAlia) {
    this.getQueryEntityMetaData().setContainTableAlia(true);

    final EntityInfo entityInfo = new EntityInfo();
    BeanUtils.copyProperties(getEntityInfo(), entityInfo);
    setEntityInfo(entityInfo);

    final QueryEntityMetaData<T> metaData = new QueryEntityMetaData<>();
    BeanUtils.copyProperties(getQueryEntityMetaData(), metaData);
    setQueryEntityMetaData(metaData);
    if (genAlia) {
      // 我们需要为子查询设置表别名
      final String alia = randomString(5);
      metaData.setFromTable(metaData.getTableName() + " " + alia);
      metaData.setTableAlia(alia);
    }
    return this;
  }

  private String randomString(int count) {
    final Random random = new Random();
    char[] chars = new char[count];
    int i = 0;
    while (i < count) chars[i++] = alphabets[random.nextInt(26)];
    return String.valueOf(chars);
  }

  public String getString() {
    // if (!containLogicNotDelete) {
    //   appendLogicNotDelete();
    //   containLogicNotDelete = true;
    // }
    return String.join("", conditionSql);
  }

  public List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex) {
    // if (!containLogicNotDelete) {
    //   appendLogicNotDelete();
    //   containLogicNotDelete = true;
    // }
    return valueTypes.stream()
        .map(
            valueType ->
                (Consumer<PreparedStatement>)
                    ps -> {
                      try {
                        ps.setObject(
                            startIndex.getAndIncrement(),
                            TypeHandlerHelper.toJdbcValue(valueType, ps));
                      } catch (SQLException e) {
                        throw CommonException.of(e);
                      }
                    })
        .collect(toCollection(LinkedList::new));
  }

  public List<Object> getOriginValues() {
    return valueTypes.stream()
        .map(ValueType::getOriginVal)
        .collect(Collectors.toCollection(LinkedList::new));
  }

  public boolean isEmpty() {
    return valueTypes.size() <= 0;
  }

  @Override
  public final Condition<T> eq(@Nonnull final Field field, final Object value) {
    return eq(true, field, value);
  }

  @Override
  public final Condition<T> eq(
      final boolean condition, @Nonnull final Field field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(EntityUtils.fieldToColumn(field))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    return this;
  }

  @Override
  public final Condition<T> in(@Nonnull final Field field, @Nonnull final Collection<?> values) {
    return in(true, field, values);
  }

  @Override
  public final Condition<T> in(
      final boolean condition, @Nonnull final Field field, @Nonnull final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(EntityUtils.fieldToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(SEMICOLON.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    return this;
  }

  @Override
  public synchronized Condition<T> appendLogicNotDelete() {
    final LogicDeleteProp logicDeleteProp = getEntityInfo().getLogicDeleteProp();
    final EntityColumn logicDeletePropColumn = getEntityInfo().getLogicDeletePropColumn();
    // 启用逻辑删除且当前未包含该条件(这个有必要?)
    if (logicDeleteProp.isEnable()
        && conditionSql.stream()
            .noneMatch(condition -> condition.matches(logicDeletePropColumn.getName() + "[ ]+="))) {
      final Field logicDeleteField = logicDeletePropColumn.getField();
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(EntityUtils.fieldToColumn(logicDeleteField))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(logicDeleteProp.isNotDelete())));
      valueTypes.add(ValueType.of(logicDeleteProp.isNotDelete(), logicDeleteField));
    }
    return this;
  }
}
