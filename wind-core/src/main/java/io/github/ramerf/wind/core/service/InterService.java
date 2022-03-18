package io.github.ramerf.wind.core.service;

import io.github.ramerf.wind.core.executor.Dao;
import io.github.ramerf.wind.core.util.EntityUtils;
import java.io.Serializable;

/**
 * The interface Inter service.
 *
 * @param <T> the type parameter
 * @author ramer
 * @since 2020 /1/5
 */
public interface InterService<T, ID extends Serializable> {

  /**
   * Gets update.
   *
   * @return the update
   */
  Dao getDao();

  /** 获取service操作的实体. */
  default Class<T> getPoJoClass() {
    return EntityUtils.getPoJoClass(this);
  }
}
