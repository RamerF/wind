package io.github.ramerf.wind.core.executor;

import io.github.ramerf.wind.core.support.EntityInfo;
import javax.annotation.Nonnull;

/**
 * 执行数据库操作.
 *
 * @author ramer
 * @since 15 /08/2020
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
}
