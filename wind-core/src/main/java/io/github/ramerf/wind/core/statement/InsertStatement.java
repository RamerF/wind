package io.github.ramerf.wind.core.statement;

import io.github.ramerf.wind.core.condition.Predicate.SqlOperator;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.util.*;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.entity.constant.Constant.SEMICOLON;

/**
 * 查询工厂,支持通用增删改查.
 *
 * @author Tang Xiaofeng
 * @since 2020/1/13
 */
@Slf4j
public class InsertStatement {
  private StringBuilder insertString = new StringBuilder();
  private StringBuilder columnString = new StringBuilder();

  public static InsertStatement getInstance() {
    return new InsertStatement();
  }

  public <T extends AbstractEntity> InsertColumn insert(Class<T> clazz) {
    return insert(EntityUtils.getTableName(clazz));
  }

  public InsertColumn insert(final String tableName) {
    insertString.append(tableName);
    return new InsertColumn(this);
  }

  public static class InsertColumn {
    private InsertStatement insertStatement;

    public InsertColumn(final InsertStatement insertStatement) {
      this.insertStatement = insertStatement;
    }

    public InsertValue columns(List<String> rowCols) {
      insertStatement.columnString.append(
          SqlOperator.PARENTHESIS_FORMAT.format(
              rowCols.stream()
                  .map(StringUtils::camelToUnderline)
                  .collect(Collectors.joining(SEMICOLON))));
      return new InsertValue(insertStatement);
    }

    // 这里需要一个批量构建values的方法,即valueBatch,未写完.
    public static class InsertValue {
      private StringBuilder valueString = new StringBuilder();
      private InsertStatement insertStatement;
      private List<Object> rowValues;

      public InsertValue(List<Object> rowValues) {
        this.rowValues = rowValues;
      }

      public static InsertValueBuilder builder() {
        return new InsertValueBuilder();
      }

      InsertValue(final InsertStatement insertStatement) {
        this.insertStatement = insertStatement;
      }

      // TODO-WARN 这里处理下特殊的数据类型,比如:数组/集合
      // 目前特殊数据类型,只能先拼接好,如数组传递'{1,2}'
      public InsertValue values(List<InsertValue> ts) {
        valueString.append(
            ts.stream().map(InsertValue::getString).collect(Collectors.joining(SEMICOLON)));
        return this;
      }

      // 拼接结果: (1,'name1'),(2,'name2')
      private String getString() {
        if (CollectionUtils.isEmpty(rowValues)) {
          throw CommonException.of("值不能为空,请调用value方法设置值");
        }
        return SqlOperator.PARENTHESIS_FORMAT.format(
            rowValues.stream()
                .map(SqlHelper::toPreFormatSqlVal)
                .collect(Collectors.joining(SEMICOLON)));
      }

      public String getInsertStr() {
        final StringBuilder string = insertStatement.insertString;
        if (log.isDebugEnabled()) {
          log.debug("getInsertStr:string[{}]", string);
        }
        return string.toString();
      }

      public String getColumnStr() {
        final StringBuilder string = insertStatement.columnString;
        if (log.isDebugEnabled()) {
          log.debug("getColumnStr:string[{}]", string);
        }
        return string.toString();
      }

      public String getValuesStr() {
        if (log.isDebugEnabled()) {
          log.debug("getValuesStr:valueString[{}]", valueString);
        }
        //      return valueString.toString();
        return valueString.toString();
      }

      public static class InsertValueBuilder {
        private List<InsertValue> values = new LinkedList<>();

        public InsertValueBuilder value(final Object... value) {
          return value(true, value);
        }

        public InsertValueBuilder value(final boolean condition, final Object... rowValue) {
          if (condition) {
            //          values.add(new InsertValue(rowValue));
          }
          return this;
        }

        // 拼接结果: (1,'name1'),(2,'name2')
        public String build() {
          if (Objects.isNull(values)) {
            throw CommonException.of("值不能为空,请调用value方法设置值");
          }
          return SqlOperator.PARENTHESIS_FORMAT.format(
              values.stream()
                  .map(SqlHelper::toPreFormatSqlVal)
                  .collect(Collectors.joining(SEMICOLON)));
        }
      }
    }
  }
}
