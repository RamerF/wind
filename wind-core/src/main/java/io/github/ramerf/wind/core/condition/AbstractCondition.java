package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.*;
import io.github.ramerf.wind.core.handler.typehandler.TypeHandlerHelper.ValueType;
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
import lombok.*;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.*;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;
import static java.util.stream.Collectors.toCollection;

/**
 * 条件构造.
 *
 * @param <POJO> the type parameter
 * @param <CONDITION> 当前对象
 * @since 2020.09.16
 * @author ramer
 */
@Slf4j
public abstract class AbstractCondition<POJO, CONDITION extends AbstractCondition<POJO, CONDITION>>
    implements Condition<POJO, CONDITION> {

  private Class<POJO> clazz;

  /** where后的字符串,参数占位符为 ?. */
  protected final List<String> conditionSql = new LinkedList<>();

  protected final List<String> groupBySql = new LinkedList<>();

  /** 占位符对应的值. */
  @Getter protected final List<ValueType> valueTypes = new LinkedList<>();

  private static final char[] alphabets =
      new char[] {
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
      };

  protected AbstractCondition() {}

  public AbstractCondition(@Nonnull final Class<POJO> clazz) {
    this.clazz = clazz;
  }

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  @Override
  public CONDITION and(final boolean condition, final String sql) {
    if (condition && StringUtils.nonEmpty(sql)) {
      conditionSql.add((conditionSql.size() > 0 ? AND.operator : "").concat(sql));
    }
    //noinspection unchecked
    return (CONDITION) this;
  }

  /** 直接拼接sql,括号需要手动加.如: {@code (id=1 and name like 'ramer%')} */
  @Override
  public CONDITION or(final boolean condition, final String sql) {
    if (condition && StringUtils.nonEmpty(sql)) {
      conditionSql.add((conditionSql.size() > 0 ? OR.operator : "").concat(sql));
    }
    //noinspection unchecked
    return (CONDITION) this;
  }

  @Override
  public String getString() {
    if (groupBySql.isEmpty()) {
      return String.join("", conditionSql);
    }
    return String.join("", conditionSql)
        .concat(GROUP_BY.operator)
        .concat(String.join(",", groupBySql));
  }

  @Override
  public String toString() {
    return getString();
  }

  @Override
  public List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex) {
    return valueTypes.stream()
        .map(
            valueType ->
                (Consumer<PreparedStatement>)
                    ps -> {
                      try {
                        final int index = startIndex.getAndIncrement();
                        final Object jdbcValue = TypeHandlerHelper.toJdbcValue(valueType, ps);
                        if (log.isDebugEnabled()) {
                          log.debug(
                              "params:[index:{},originValue:{},value:{}]",
                              index,
                              valueType.getOriginVal(),
                              jdbcValue);
                        }
                        ps.setObject(index, jdbcValue);
                      } catch (SQLException e) {
                        throw new CommonException(e);
                      }
                    })
        .collect(toCollection(LinkedList::new));
  }

  @Override
  public List<Object> getOriginValues() {
    return valueTypes.stream()
        .map(ValueType::getOriginVal)
        .collect(Collectors.toCollection(LinkedList::new));
  }

  @Override
  public boolean isEmpty() {
    return valueTypes.isEmpty();
  }

  @Override
  public final CONDITION eq(
      final boolean condition, @Nonnull final Field field, final Object value) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(EntityUtils.fieldToColumn(field))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(value)));
      valueTypes.add(ValueType.of(value, field));
    }
    //noinspection unchecked
    return (CONDITION) this;
  }

  @Override
  public final CONDITION in(
      final boolean condition, @Nonnull final Field field, @Nonnull final Collection<?> values) {
    if (condition) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(EntityUtils.fieldToColumn(field))
              .concat(
                  String.format(
                      MatchPattern.IN.operator,
                      values.stream()
                          .map(SqlHelper::toPreFormatSqlVal)
                          .collect(Collectors.joining(COMMA.operator)))));
      values.forEach(value -> valueTypes.add(ValueType.of(value, field)));
    }
    //noinspection unchecked
    return (CONDITION) this;
  }

  @Override
  public final synchronized CONDITION appendLogicNotDelete() {
    EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
    final LogicDeleteProp logicDeleteProp = entityInfo.getLogicDeleteProp();
    final EntityColumn logicDeletePropColumn = entityInfo.getLogicDeletePropColumn();
    // 启用逻辑删除且当前未包含该条件(这个有必要?)
    if (logicDeleteProp.isEnable()
        && conditionSql.stream()
            .noneMatch(condition -> condition.matches(logicDeletePropColumn.getName() + "[ ]+="))) {
      final Field logicDeleteField = logicDeletePropColumn.getField();
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(EntityUtils.fieldToColumn(logicDeleteField))
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(logicDeleteProp.isNotDelete())));
      valueTypes.add(ValueType.of(logicDeleteProp.isNotDelete(), logicDeleteField));
    }
    //noinspection unchecked
    return (CONDITION) this;
  }
}
