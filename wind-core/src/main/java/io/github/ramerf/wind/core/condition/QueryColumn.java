package io.github.ramerf.wind.core.condition;

import io.github.ramerf.wind.core.condition.function.*;
import io.github.ramerf.wind.core.config.*;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.math.BigDecimal;
import java.util.*;
import javax.annotation.Nonnull;
import lombok.*;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.AS;
import static io.github.ramerf.wind.core.condition.Condition.SqlOperator.DOT;
import static java.util.stream.Collectors.joining;

/**
 * sql查询列定义.<br>
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2019/12/26
 */
@Slf4j
@EqualsAndHashCode
@SuppressWarnings("UnusedReturnValue")
public class QueryColumn<T> {
  @Getter
  @Setter(AccessLevel.PROTECTED)
  private QueryEntityMetaData<T> queryEntityMetaData = new QueryEntityMetaData<>();

  @Getter(AccessLevel.PROTECTED)
  @Setter(AccessLevel.PROTECTED)
  private EntityInfo entityInfo;

  public static <T> QueryColumn<T> of(final Class<T> clazz) {
    return of(clazz, null, null);
  }

  public static <T> QueryColumn<T> of(final Class<T> clazz, final String tableAlia) {
    return of(clazz, null, tableAlia);
  }

  public static <T> QueryColumn<T> of(final String tableName) {
    return of(null, tableName, null);
  }

  public static <T> QueryColumn<T> of(final String tableName, final String tableAlia) {
    return of(null, tableName, tableAlia);
  }

  private static <T> QueryColumn<T> of(final Class<T> clazz, String tableName, String tableAlia) {
    if (clazz == null && tableName == null && tableAlia == null) {
      throw new IllegalArgumentException("[clazz,tableName,tableAlia]不能同时为空");
    }
    final WindConfiguration configuration = AppContextInject.getBean(WindConfiguration.class);
    final QueryColumn<T> queryColumn = new QueryColumn<>(EntityInfo.of(configuration));
    if (clazz != null) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
      // 如果tableName不为空,需要覆盖entityInfo的值.传入的tableName优先级最高,因为支持使用不相关的类查询表
      if (tableName != null) {
        entityInfo.setName(tableName);
      } else {
        tableName = entityInfo.getName();
      }
      queryColumn.setEntityInfo(entityInfo);
    }

    final QueryEntityMetaData<T> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    queryEntityMetaData.setClazz(clazz);
    queryEntityMetaData.setTableName(tableName);
    tableAlia = tableAlia == null ? tableName : tableAlia;
    queryEntityMetaData.setTableAlia(tableAlia);
    String fromTable = tableName;
    if (tableAlia != null && !tableAlia.equals(tableName)) {
      fromTable = tableName.concat(" ").concat(tableAlia);
    }
    queryEntityMetaData.setFromTable(fromTable);
    return queryColumn;
  }

  private QueryColumn(final EntityInfo entityInfo) {
    setEntityInfo(entityInfo);
  }

  /**
   * 查询列.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> col(final IFunction<T, ?> function) {
    return col(function, (String) null);
  }

  /**
   * 查询列.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> col(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, null);
  }

  /**
   * sql函数.
   *
   * @see BaseSqlFunction
   * @see CaseWhenSqlFunction
   */
  public QueryColumn<T> col(final SqlFunction sqlFunction) {
    return col(sqlFunction.string());
  }

  /**
   * 自定义查询表达式.
   *
   * <p>示例:col("id,case sex when 1 then '男' when 2 then '女' else '未知' end")
   *
   * @param sql 查询列表达式
   * @return the query column
   */
  public QueryColumn<T> col(@Nonnull final String sql) {
    if (!sql.isEmpty()) {
      final QueryEntityMetaData<T> metaData = getQueryEntityMetaData();
      metaData.queryAlias.add(QueryAlia.of(sql));
    }
    return this;
  }

  /**
   * 对指定字段做统计.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> count(final IFunction<T, ?> function) {
    return count(function, null);
  }

  /**
   * 对指定字段做统计.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> count(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.COUNT);
  }

  /**
   * 对指定字段求和.
   *
   * @param function the function
   * @return the query column
   * @see #sum(IFunction, String) #sum(IFunction, String)
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function) {
    return sum(function, null);
  }

  /**
   * 对指定字段求和. 不清楚返回类型的情况下使用{@link BigDecimal}
   *
   * <pre>
   * <b>注意:该列的返回类型与数据库对应关系</b>
   * <b>java           jdbc</b>
   * BigDecimal     bigint/numeric/decimal
   * Double         double/float
   *
   * </pre>
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> sum(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.SUM);
  }

  /** 对指定表达式求和,比如sum(case when...) */
  public QueryColumn<T> sum(final SqlFunction sqlFunction) {
    return col(AggregateSqlFunction.SUM.string(sqlFunction.string()));
  }

  /**
   * 对指定字段求最大值.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> max(final IFunction<T, ?> function) {
    return max(function, null);
  }

  /**
   * 对指定字段求最大值.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> max(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.MAX);
  }

  /**
   * 对指定字段求最小值.
   *
   * @param function the function
   * @return the query column
   */
  public QueryColumn<T> min(final IFunction<T, ?> function) {
    return min(function, null);
  }

  /**
   * 对指定字段求最小值.
   *
   * @param function the function
   * @param alia the alia
   * @return the query column
   */
  public QueryColumn<T> min(final IFunction<T, ?> function, final String alia) {
    return col(function, alia, AggregateSqlFunction.MIN);
  }

  /** 添加查询对象(列/聚合函数). */
  public QueryColumn<T> col(final IFunction<T, ?> function, final SqlFunction sqlFunction) {
    return col(function, null, sqlFunction);
  }

  /** 添加查询对象(列/聚合函数). */
  public QueryColumn<T> col(
      final IFunction<T, ?> function, final String alia, final SqlFunction sqlFunction) {
    final QueryEntityMetaData<T> metaData = getQueryEntityMetaData();
    metaData.queryAlias.add(
        QueryAlia.of(
            function, alia, metaData.getTableName(), metaData.getTableAlia(), sqlFunction));
    return this;
  }

  public String getString() {
    return getString(false);
  }

  /** 是否包含列别名,单表时不需要包含别名,传false. */
  public String getString(final boolean containAlia) {
    final QueryEntityMetaData<T> metaData = getQueryEntityMetaData();
    if (CollectionUtils.isEmpty(metaData.queryAlias)) {
      EntityHelper.getEntityInfo(metaData.clazz).getEntityColumns().stream()
          .filter(EntityColumn::isSupported)
          .forEach(this::col);
    }
    return metaData.queryAlias.stream()
        .map(o -> toColumnWithAlia(o, containAlia))
        .collect(joining(","));
  }

  /** 添加查询对象(列/聚合函数). */
  private QueryColumn<T> col(final EntityColumn entityColumn) {
    getQueryEntityMetaData()
        .queryAlias
        .add(
            QueryAlia.of(
                entityColumn.getField().getName(),
                entityColumn.getName(),
                entityColumn.getName(),
                getQueryEntityMetaData().getTableName(),
                getQueryEntityMetaData().getTableAlia()));
    return this;
  }

  /** 增加额外的表别名前缀 */
  private static String toColumnWithAlia(final QueryAlia queryAlia, final boolean containAlia) {
    final String customSql = queryAlia.getCustomSql();
    if (customSql != null) {
      return customSql;
    }

    final String alia = queryAlia.getColumnAlia();
    final String name = queryAlia.getColumnName();
    final String tableAlias = queryAlia.getTableAlia();

    final SqlFunction sqlFunction = queryAlia.getSqlFunction();
    final String queryName =
        sqlFunction != null
            ? sqlFunction.string(tableAlias.concat(DOT.operator()).concat(name))
            : tableAlias.concat(DOT.operator()).concat(name);
    return containAlia ? queryName.concat(AS.operator()).concat(alia) : queryName;
  }

  @Data
  public static class QueryAlia {
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
      if (sqlFunction != null) {
        sb.append(sqlFunction.string(columnAlia));
      } else {
        sb.append(" ".concat(columnAlia));
      }

      return sb.toString();
    }
  }
}
