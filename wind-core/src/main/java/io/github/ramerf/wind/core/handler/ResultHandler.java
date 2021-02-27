package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.Data;

/**
 * 返回结果转换.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author ramer
 * @since 2019 /12/27
 * @see AbstractResultHandler
 */
public interface ResultHandler<T, E> {

  /**
   * Handler e.
   *
   * @param t the t
   * @return the e
   */
  E handle(T t);

  /**
   * Handler list.
   *
   * @param ts the ts
   * @return the list
   */
  List<E> handle(List<T> ts);

  /** TODO POST 实现语法: case when 时,QueryAlia.case(1).then().case(2).then().else() */
  @Data
  class QueryAlia {
    /** 字段名. */
    private String fieldName;
    /** 列名(字段名下划线分割). */
    private String columnName;
    /** 列别名. */
    private String columnAlia;
    /** 表名: @TableInfo#name &gt; 类名(驼封转下划线). */
    private String tableName;
    /** 表别名. */
    private String tableAlia;
    /** sql函数. */
    private SqlFunction sqlFunction;

    /** 自定义sql,用于扩展支持. */
    private String customSql;

    private QueryAlia() {}

    /** {@link BeanFunction#getImplClassFullPath()} :tableName */
    private static Map<String, WeakReference<String>> TABLE_NAME_MAP = new HashMap<>();

    public static QueryAlia of(BeanFunction function, final String columnAlia, String tableAlia) {
      return of(function, columnAlia, null, tableAlia, null);
    }

    public static QueryAlia of(
        BeanFunction function,
        final String columnAlia,
        final String tableName,
        final String tableAlia) {
      return of(function, columnAlia, tableName, tableAlia, null);
    }

    public static QueryAlia of(@Nonnull final String customSql) {
      final QueryAlia queryAlia = new QueryAlia();
      queryAlia.setCustomSql(customSql);
      return queryAlia;
    }

    public static QueryAlia of(
        @Nonnull final String fieldName,
        @Nonnull final String columnName,
        final String columnAlia,
        @Nonnull String tableName,
        @Nonnull final String tableAlia) {
      final QueryAlia queryAlia = new QueryAlia();
      queryAlia.setFieldName(fieldName);
      queryAlia.setColumnName(columnName);
      queryAlia.setColumnAlia(columnAlia);
      if (StringUtils.isEmpty(columnAlia)) {
        queryAlia.setColumnAlia(columnName);
      }
      queryAlia.setTableName(tableName);
      queryAlia.setTableAlia(tableAlia);
      return queryAlia;
    }

    public static QueryAlia of(
        BeanFunction function,
        final String columnAlia,
        String tableName,
        final String tableAlia,
        final SqlFunction sqlFunction) {
      final QueryAlia queryAlia = new QueryAlia();

      final String fieldName = function.getField().getName();
      queryAlia.setFieldName(fieldName);

      final String columnName = function.getColumn();
      queryAlia.setColumnName(columnName);
      /*
       别名逻辑:
       1. 别名
       2. 别名为空时,如果用户定义的列名(@TableColumn.name)和下划线格式的字段名不相等,使用字段对应的下划线表示(解决字段名和列名不对应时,查询字段为空)
      */
      final String underlineField = StringUtils.camelToUnderline(fieldName);
      queryAlia.setColumnAlia(
          StringUtils.nonEmpty(columnAlia)
              ? columnAlia
              : columnName.equals(fieldName) ? columnName : underlineField);

      final String classFullPath = function.getImplClassFullPath();
      final String className = function.getImplClassName();
      if (StringUtils.isEmpty(tableName)) {
        tableName =
            Optional.ofNullable(TABLE_NAME_MAP.get(classFullPath))
                .map(Reference::get)
                .orElseGet(
                    () -> {
                      final String name =
                          EntityUtils.getTableName(BeanUtils.getClazz(classFullPath));
                      TABLE_NAME_MAP.put(classFullPath, new WeakReference<>(name));
                      return name;
                    });
      }
      queryAlia.setTableName(tableName);
      queryAlia.setTableAlia(StringUtils.isEmpty(tableAlia) ? tableName : tableAlia);
      queryAlia.setSqlFunction(sqlFunction);
      return queryAlia;
    }

    public static QueryAlia of(
        final String fieldName, final String tableAlia, final String tableName) {
      final QueryAlia columnAlia = new QueryAlia();
      columnAlia.setFieldName(fieldName);
      final String column = StringUtils.camelToUnderline(fieldName);
      columnAlia.setColumnName(column);
      columnAlia.setColumnAlia(column);
      columnAlia.setTableAlia(tableAlia);
      columnAlia.setTableName(tableName);
      return columnAlia;
    }

    @SuppressWarnings("unused")
    public String getQueryString() {
      StringBuilder sb = new StringBuilder();
      // 这里可能会出现较复杂逻辑,不要改为三目运算符
      if (Objects.nonNull(sqlFunction)) {
        sb.append(sqlFunction.string(columnAlia));
      } else {
        sb.append(" ".concat(columnAlia));
      }

      return sb.toString();
    }
  }
}
