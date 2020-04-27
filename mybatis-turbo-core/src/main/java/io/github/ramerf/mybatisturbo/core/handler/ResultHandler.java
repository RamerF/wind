package io.github.ramerf.mybatisturbo.core.handler;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.mybatisturbo.core.conditions.IFunction;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.util.*;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.*;
import lombok.Data;
import org.apache.ibatis.reflection.property.PropertyNamer;

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

    private QueryAlia() {}

    /** {@link java.lang.invoke.SerializedLambda#getInstantiatedMethodType()}:tableAlia */
    private static Map<String, WeakReference<String>> tableAliaMap = new HashMap<>();
    /** {@link java.lang.invoke.SerializedLambda#getInstantiatedMethodType()}:tableName */
    private static Map<String, WeakReference<String>> tableNameMap = new HashMap<>();

    public static <T extends AbstractEntity> QueryAlia of(
        IFunction<T, ?> function, final String alia, String tableAlia) {
      final QueryAlia queryAlia = new QueryAlia();

      final SerializedLambda lambda = LambdaUtils.serializedLambda(function);
      final String fieldName = PropertyNamer.methodToProperty(lambda.getImplMethodName());
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
  }
}
