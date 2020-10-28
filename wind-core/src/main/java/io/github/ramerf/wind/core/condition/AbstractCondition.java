package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.config.LogicDeleteProp;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.function.IConsumer;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper;
import io.github.ramerf.wind.core.helper.TypeHandlerHelper.ValueType;
import io.github.ramerf.wind.core.support.EntityInfo;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import net.bytebuddy.utility.RandomString;
import org.springframework.beans.BeanUtils;

import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.AND;
import static io.github.ramerf.wind.core.condition.Predicate.SqlOperator.DOT;
import static io.github.ramerf.wind.core.helper.SqlHelper.toPreFormatSqlVal;
import static java.util.stream.Collectors.toCollection;

/**
 * 条件构造.
 *
 * @since 2020.09.16
 * @author Tang Xiaofeng
 */
@Slf4j
@ToString
public abstract class AbstractCondition<T extends AbstractEntityPoJo> extends AbstractQueryEntity<T>
    implements ICondition<T> {
  /** where后的字符串,参数占位符为 ?. */
  final List<String> conditionSql = new LinkedList<>();
  /** 占位符对应的值. */
  final List<ValueType> valueTypes = new LinkedList<>();

  private boolean containLogicNotDelete = false;

  /** 获取实例.通常是默认构造器. */
  abstract AbstractCondition<T> of();

  public AbstractCondition<T> condition(final boolean genAlia) {
    this.getQueryEntityMetaData().setContainTableAlia(true);

    final AbstractCondition<T> condition = of();
    final EntityInfo entityInfo = new EntityInfo();
    BeanUtils.copyProperties(getEntityInfo(), entityInfo);
    condition.setEntityInfo(entityInfo);

    final QueryEntityMetaData<T> metaData = new QueryEntityMetaData<>();
    BeanUtils.copyProperties(getQueryEntityMetaData(), metaData);
    condition.setQueryEntityMetaData(metaData);
    if (genAlia) {
      // 我们需要为子查询设置表别名
      final String alia = RandomString.make(5);
      metaData.setFromTable(metaData.getTableName() + " " + alia);
      metaData.setTableAlia(alia);
    }
    return condition;
  }

  public String getString() {
    if (!containLogicNotDelete) {
      appendLogicNotDelete();
      containLogicNotDelete = true;
    }
    return String.join("", conditionSql);
  }

  private synchronized void appendLogicNotDelete() {
    final LogicDeleteProp logicDeleteProp = getEntityInfo().getLogicDeleteProp();
    if (logicDeleteProp.isEnable()) {
      conditionSql.add(
          (conditionSql.size() > 0 ? AND.operator : "")
              .concat(getQueryEntityMetaData().getTableAlia())
              .concat(DOT.operator)
              .concat(logicDeleteProp.getColumn())
              .concat(MatchPattern.EQUAL.operator)
              .concat(toPreFormatSqlVal(logicDeleteProp.isNotDelete())));
      final IConsumer<AbstractEntityPoJo, Boolean> beanFunction = AbstractEntityPoJo::setDeleted;
      valueTypes.add(ValueType.of(logicDeleteProp.isNotDelete(), beanFunction));
    }
  }

  public List<Consumer<PreparedStatement>> getValues(final AtomicInteger startIndex) {
    if (!containLogicNotDelete) {
      appendLogicNotDelete();
      containLogicNotDelete = true;
    }
    return valueTypes.stream()
        .map(
            valueType ->
                (Function<PreparedStatement, Object>)
                    ps -> TypeHandlerHelper.toJdbcValue(valueType, ps))
        .map(
            function ->
                (Consumer<PreparedStatement>)
                    ps -> {
                      try {
                        ps.setObject(startIndex.getAndIncrement(), function.apply(ps));
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

  /** 属性匹配模式 */
  public enum MatchPattern {
    /** = */
    EQUAL("="),
    /** != */
    NOT_EQUAL("!="),
    /** &gt; */
    GREATER(">"),
    /** &gt;= */
    GE(">="),
    /** &lt; */
    LESS("<"),
    /** &lt;= */
    LE("<="),
    LIKE_PLAIN(" LIKE %s "),
    /** LIKE %criteria% */
    LIKE(" LIKE '%%%s%%'"),
    /** LIKE %criteria */
    LIKE_LEFT(" LIKE '%%%s'"),
    /** LIKE criteria% */
    LIKE_RIGHT(" LIKE '%s%%'"),
    /** NOT LIKE %criteria% */
    NOT_LIKE_PLAIN(" NOT LIKE %s "),
    NOT_LIKE(" NOT LIKE '%%%s%%'"),
    /** BETWEEN start AND end */
    BETWEEN(" BETWEEN %s and %s"),
    /** NOT BETWEEN start AND end */
    NOT_BETWEEN(" NOT BETWEEN %s and %s"),
    /** IS NULL */
    IS_NULL(" IS NULL"),
    /** IS NOT NULL */
    IS_NOT_NULL(" IS NOT NULL"),
    /** EXISTS */
    EXISTS(" EXISTS(%s)"),
    /** NOT EXISTS */
    NOT_EXISTS(" NOT EXISTS(%s)"),
    /** IN () */
    IN(" IN (%s)"),
    /** NOT IN () */
    NOT_IN(" NOT IN (%s)"),
    /** &gt;ANY() */
    GREATER_ANY(" >ANY(%s)"),
    /** &gt;=ANY() */
    GE_ANY(" >=ANY(%s)"),
    /** &lt;ANY() */
    LESS_ANY(" <ANY(%s)"),
    /** &lt;=ANY() */
    LE_ANY(" <=ANY(%s)");

    final String operator;

    public String operator() {
      return operator;
    }

    MatchPattern(final String operator) {
      this.operator = operator;
    }
  }
}
