package io.github.ramerf.mybatisturbo.core.factory;

import com.baomidou.mybatisplus.annotation.TableName;
import io.github.ramerf.mybatisturbo.core.conditions.QueryColumn;
import io.github.ramerf.mybatisturbo.core.conditions.QueryEntityMetaData;
import io.github.ramerf.mybatisturbo.core.config.AppContextInject;
import io.github.ramerf.mybatisturbo.core.config.MybatisTurboConfiguration;
import io.github.ramerf.mybatisturbo.core.entity.AbstractEntity;
import io.github.ramerf.mybatisturbo.core.entity.constant.Constant;
import io.github.ramerf.mybatisturbo.core.exception.CommonException;
import io.github.ramerf.mybatisturbo.core.util.StringUtils;
import java.util.Objects;
import java.util.Optional;
import javax.annotation.Resource;

/**
 * @author Tang Xiaofeng
 * @since 2019/12/28
 */
public class QueryColumnFactory {
  @Resource private MybatisTurboConfiguration configuration;

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
        QueryColumn.of(AppContextInject.getBean(MybatisTurboConfiguration.class));
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
