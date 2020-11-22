package io.github.ramerf.wind.core.factory;

import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.QueryEntityMetaData;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.helper.EntityHelper;
import io.github.ramerf.wind.core.support.EntityInfo;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Objects;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
public class QueryColumnFactory {

  public static <T extends AbstractEntityPoJo<T, ?>> QueryColumn<T> fromClass(
      final Class<T> clazz) {
    return getInstance(clazz, null, null);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> QueryColumn<T> fromClassAndTableAlia(
      final Class<T> clazz, final String tableAlia) {
    return getInstance(clazz, null, tableAlia);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> QueryColumn<T> fromTableName(
      final String tableName) {
    return getInstance(null, tableName, null);
  }

  public static <T extends AbstractEntityPoJo<T, ?>> QueryColumn<T> fromTableNameAndAlia(
      final String tableName, final String tableAlia) {
    return getInstance(null, tableName, tableAlia);
  }

  private static <T extends AbstractEntityPoJo<T, ?>> QueryColumn<T> getInstance(
      final Class<T> clazz, String tableName, String tableAlia) {
    final QueryColumn<T> queryColumn =
        QueryColumn.of(AppContextInject.getBean(WindConfiguration.class));
    if (Objects.isNull(clazz) && StringUtils.isEmpty(tableName) && StringUtils.isEmpty(tableAlia)) {
      throw CommonException.of("[clazz,tableName,tableAlia]不能同时为空");
    }
    final QueryEntityMetaData<T> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    queryEntityMetaData.setClazz(clazz);
    if (clazz != null) {
      final EntityInfo entityInfo = EntityHelper.getEntityInfo(clazz);
      // 如果tableName不为空,需要覆盖entityInfo的值.传入的tableName优先级最高,因为支持使用不相关的类查询表
      if (StringUtils.nonEmpty(tableName)) {
        entityInfo.setName(tableName);
      } else {
        tableName = entityInfo.getName();
      }
      queryColumn.setEntityInfo(entityInfo);
    }
    queryEntityMetaData.setTableName(tableName);
    tableAlia = StringUtils.isEmpty(tableAlia) ? tableName : tableAlia;
    queryEntityMetaData.setTableAlia(tableAlia);
    String fromTable = tableName;
    if (StringUtils.nonEmpty(tableAlia) && !tableAlia.equals(tableName)) {
      fromTable = tableName.concat(" ").concat(tableAlia);
    }
    queryEntityMetaData.setFromTable(fromTable);
    return queryColumn;
  }
}
