package io.github.ramerf.wind.core.cache;

import io.github.ramerf.wind.core.config.WindConfiguration;
import io.github.ramerf.wind.core.entity.pojo.AbstractEntityPoJo;
import io.github.ramerf.wind.core.executor.Executor.SqlParam;
import io.github.ramerf.wind.core.helper.SqlHelper;
import io.github.ramerf.wind.core.util.StringUtils;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.extern.slf4j.Slf4j;

/**
 * 用于查询redis缓存.
 *
 * @author Tang Xiaofeng
 * @since 2020 /5/26
 */
@Slf4j
public abstract class AbstractCache implements RedisCache {
  final WindConfiguration configuration;

  AbstractCache(WindConfiguration configuration) {
    this.configuration = configuration;
  }

  @Override
  public final WindConfiguration getConfiguration() {
    return configuration;
  }

  /**
   * 清除对应key的数据.
   *
   * @param clazz {@link AbstractEntityPoJo}
   */
  @Override
  public void clear(@Nonnull final Class<?> clazz) {
    final String key = getFixedKeyPrefix(clazz);
    if (log.isDebugEnabled()) {
      log.debug("clear:Clear cache[{}]", key);
    }
    if (isKeyExist(key)) {
      clear(key);
    }
  }

  @Override
  public String generateKey(@Nonnull final SqlParam sqlParam, @Nonnull final String methodName) {
    return getFixedKeyPrefix(sqlParam.getEntityClazz())
        + ":"
        + methodName
        + ":"
        + sqlParam.getSql()
        + ":"
        + (Objects.nonNull(sqlParam.getAggregateFunction())
            ? sqlParam.getAggregateFunction().name()
            : sqlParam.getConditions().stream()
                .flatMap(o -> o.getOriginValues().stream())
                .map(SqlHelper::toSqlString)
                .collect(Collectors.joining()));
  }

  /**
   * 获取key前缀,如果不为空,加上:后缀
   *
   * @param clazz 操作的pojo
   * @return 包含 :的key前缀
   */
  private String getFixedKeyPrefix(final Class<?> clazz) {
    String keyPrefix = getConfiguration().getCache().getKeyPrefix();
    // 如果结尾没有包含冒号,加上
    final String name = clazz == null ? "null" : clazz.getName();
    return StringUtils.isEmpty(keyPrefix)
        ? "" + name
        : keyPrefix.endsWith(":") ? keyPrefix : keyPrefix + ":" + name;
  }
}
