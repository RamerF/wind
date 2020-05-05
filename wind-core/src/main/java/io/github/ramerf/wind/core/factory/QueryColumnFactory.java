package io.github.ramerf.wind.core.factory;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.wind.core.condition.QueryColumn;
import io.github.ramerf.wind.core.condition.QueryEntityMetaData;
import io.github.ramerf.wind.core.config.AppContextInject;
import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.AbstractEntity;
import io.github.ramerf.wind.core.entity.constant.Constant;
import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Objects;
import java.util.Optional;

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

  private static <T extends AbstractEntity> QueryColumn<T> getInstance(
      final Class<T> clazz, String tableName, String tableAlia) {
    final QueryColumn<T> queryColumn =
        QueryColumn.of(AppContextInject.getBean(WindConfiguration.class));
    if (Objects.isNull(clazz) && StringUtils.isEmpty(tableName) && StringUtils.isEmpty(tableAlia)) {
      throw CommonException.of("[clazz,tableName,tableAlia]不能同时为空");
    }
    final QueryEntityMetaData<T> queryEntityMetaData = queryColumn.getQueryEntityMetaData();
    queryEntityMetaData.setClazz(clazz);
    if (StringUtils.isEmpty(tableName)) {
      tableName =
          Optional.ofNullable(clazz.getAnnotation(TableName.class))
              .map(TableName::value)
              .orElse(StringUtils.camelToUnderline(clazz.getSimpleName()));
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
