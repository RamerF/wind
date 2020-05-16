package io.github.ramerf.wind.core.handler;

import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.function.BeanFunction;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.*;
import javax.persistence.Entity;
import lombok.Data;

/**
 * 返回结果转换.
 *
 * @param <T> the type parameter
 * @param <E> the type parameter
 * @author Tang Xiaofeng
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

  @Data
  class QueryAlia {
    /*
     TODO: 实现语法: case when 时,QueryAlia.case(1).then().case(2).then().else()
    */
    /** 字段名. */
    private String fieldName;
    /** 列名(字段名下划线分割). */
    private String columnName;
    /** 列别名. */
    private String columnAlia;
    /** 表名: @Entity#name > 类名(驼封转下划线). */
    private String tableName;
    /** 表别名. */
    private String tableAlia;
    /** sql函数. */
    private SqlFunction sqlFunction;

    private QueryAlia() {}

    /** {@link java.lang.invoke.SerializedLambda#getInstantiatedMethodType()}:tableAlia */
    private static Map<String, WeakReference<String>> tableAliaMap = new HashMap<>();
    /** {@link java.lang.invoke.SerializedLambda#getInstantiatedMethodType()}:tableName */
    private static Map<String, WeakReference<String>> tableNameMap = new HashMap<>();

    public static QueryAlia of(BeanFunction function, final String alia, String tableAlia) {
      return of(function, alia, tableAlia, null);
    }

    public static QueryAlia of(
        BeanFunction function, final String alia, String tableAlia, final SqlFunction sqlFunction) {
      final QueryAlia queryAlia = new QueryAlia();

      final SerializedLambda lambda = LambdaUtils.serializedLambda(function);
      final String fieldName = BeanUtils.methodToProperty(lambda.getImplMethodName());
      queryAlia.setFieldName(fieldName);

      final String columnName = EntityHelper.getColumn(function);
      queryAlia.setColumnName(columnName);
      /*
       别名逻辑:
       1. 别名
       2. 别名为空时,如果用户定义的列名(@Column.name)和下划线格式的字段名不相等,使用字段对应的下划线表示(解决字段名和列名不对应时,查询字段为空)
      */
      final String underlineField = StringUtils.camelToUnderline(fieldName);
      queryAlia.setColumnAlia(
          StringUtils.nonEmpty(alia)
              ? alia
              : columnName.equals(fieldName) ? columnName : underlineField);

      final String implClass = lambda.getImplClass();
      final String actualType = LambdaUtils.getActualType(implClass);
      queryAlia.setTableName(
          Optional.ofNullable(tableNameMap.get(implClass))
              .map(Reference::get)
              .orElseGet(
                  () -> {
                    final String actualTypePath = LambdaUtils.getActualTypePath(implClass);
                    final Class<Object> clazz = BeanUtils.getClazz(actualTypePath);
                    final Entity entity = clazz.getAnnotation(Entity.class);
                    // 表名: @Entity#name > 类名(驼封转下划线)
                    final String tableName =
                        Objects.nonNull(entity) && StringUtils.nonEmpty(entity.name())
                            ? entity.name()
                            : StringUtils.camelToUnderline(actualType);
                    tableNameMap.put(implClass, new WeakReference<>(tableName));
                    return tableName;
                  }));
      if (StringUtils.isEmpty(tableAlia)) {
        tableAlia =
            Optional.ofNullable(tableAliaMap.get(implClass))
                .map(Reference::get)
                .orElseGet(
                    () -> {
                      final String type = StringUtils.camelToUnderline(actualType);
                      tableAliaMap.put(implClass, new WeakReference<>(type));
                      return type;
                    });
      }

      queryAlia.setTableAlia(tableAlia);
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
