package io.github.ramerf.wind.core.support.handler;

import io.github.ramerf.wind.core.support.handler.manager.HandlerManager;

/**
 * 处理动作.
 *
 * @param <T> the type parameter
 * @author zhairuihao
 * @since 2020 /4/3/003
 */
public interface FilterHandler<T> {

  /**
   * Handle.
   *
   * @param t the t
   * @param manager the manager
   */
  void handle(T t, HandlerManager<T> manager);
}
