package io.github.ramerf.wind.core.factory;

import io.github.ramerf.wind.core.annotation.TableInfo;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.QueryEntityMetaData;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
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

  public static <T extends AbstractEntity> QueryColumn<T> getInstance(final Class<T> clazz) {
    return getInstance(clazz, null, null);
  }

  public static <T extends AbstractEntity> QueryColumn<T> getInstance(
      final Class<T> clazz, final String tableAlia) {
    return getInstance(clazz, null, tableAlia);
  }

  public static <T extends AbstractEntity> QueryColumn<T> getInstance(final String tableName) {
    return getInstance(null, tableName, null);
  }

  public static <T extends AbstractEntity> QueryColumn<T> getInstance(
      final String tableName, final String tableAlia) {
    return getInstance(null, tableName, tableAlia);
  }

  public static <T extends AbstractEntity> QueryColumn<T> getInstance(
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
      if (StringUtils.isEmpty(tableName)) {
        tableName = entityInfo.getName();
      }
      // 如果使用了@TableInfo注解,即使只指定了name,其它实体相关的全局配置也会失效,好坑😓
      if (clazz.getAnnotation(TableInfo.class) != null) {
        queryColumn.setEntityInfo(entityInfo);
      }
    }
    queryEntityMetaData.setTableName(tableName);
    tableAlia = StringUtils.isEmpty(tableAlia) ? tableName : tableAlia;
    queryEntityMetaData.setTableAlia(tableAlia);
    String fromTable = tableName;
    if (StringUtils.nonEmpty(tableAlia)) {
      fromTable = tableName.concat(Constant.DEFAULT_SPLIT_SPACE).concat(tableAlia);
    }
    queryEntityMetaData.setFromTable(fromTable);
    return queryColumn;
  }
}
