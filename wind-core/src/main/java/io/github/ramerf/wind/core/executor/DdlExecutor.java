package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.dialect.Dialect;
import io.github.ramerf.wind.core.support.EntityInfo;
import javax.annotation.Nonnull;

/**
 * 数据库操作接口,不同的数据库分别实现,应该会提供一个抽象类.
 *
 * @author ramer
 * @since 15 /08/2020
 * @see DefaultDdlExecutor
 */
public interface DdlExecutor {
  /**
   * Create table.
   *
   * @param entityInfo the entity info
   */
  void createTable(@Nonnull final EntityInfo entityInfo);

  /**
   * Update table.
   *
   * @param entityInfo the entity info
   */
  void updateTable(@Nonnull final EntityInfo entityInfo);

  Dialect getDialect();
}
