package io.github.ramerf.wind.core.handler;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.wind.core.condition.function.SqlFunction;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.function.IFunction;
import io.github.ramerf.wind.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.*;
import lombok.Data;

import static io.github.ramerf.wind.core.util.BeanUtils.methodToProperty;

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
    /** 表名(注解{@link TableName}指定的名称;不存在时为类名下划线分割). */
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

    public static <T extends AbstractEntity> QueryAlia of(
        IFunction<T, ?> function, final String alia, String tableAlia) {
      return of(function, alia, tableAlia, null);
    }

    public static <T extends AbstractEntity> QueryAlia of(
        IFunction<T, ?> function,
        final String alia,
        String tableAlia,
        final SqlFunction sqlFunction) {
      final QueryAlia queryAlia = new QueryAlia();

      final SerializedLambda lambda = LambdaUtils.serializedLambda(function);
      final String fieldName = methodToProperty(lambda.getImplMethodName());
      queryAlia.setFieldName(fieldName);

      final String columnName = StringUtils.camelToUnderline(fieldName);
      queryAlia.setColumnName(columnName);
      queryAlia.setColumnAlia(StringUtils.isEmpty(alia) ? columnName : alia);

      final String methodType = lambda.getInstantiatedMethodType();
      queryAlia.setTableName(
          Optional.ofNullable(tableNameMap.get(methodType))
              .map(Reference::get)
              .orElseGet(
                  () -> {
                    final String tableName =
                        Optional.ofNullable(
                                BeanUtils.getClazz(LambdaUtils.getActualTypePath(methodType))
                                    .getAnnotation(TableName.class))
                            .map(TableName::value)
                            .orElse(
                                StringUtils.camelToUnderline(
                                    LambdaUtils.getActualType(methodType)));
                    tableNameMap.put(methodType, new WeakReference<>(tableName));
                    return tableName;
                  }));
      if (StringUtils.isEmpty(tableAlia)) {
        tableAlia =
            Optional.ofNullable(tableAliaMap.get(methodType))
                .map(Reference::get)
                .orElseGet(
                    () -> {
                      final String type =
                          StringUtils.camelToUnderline(LambdaUtils.getActualType(methodType));
                      tableAliaMap.put(methodType, new WeakReference<>(type));
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
