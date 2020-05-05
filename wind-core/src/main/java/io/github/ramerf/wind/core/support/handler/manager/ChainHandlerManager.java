package io.github.ramerf.wind.core.support.handler.manager;

import io.github.ramerf.wind.core.support.handler.FilterHandler;

import java.util.List;

/**
 * 链式处理管理器.
 *
 * @param <T> the type parameter
 * @author zhairuihao
 * @since 2020 /4/8/008
 */
public interface ChainHandlerManager<T> extends HandlerManager<T> {
  /**
   * Gets handlers.
   *
   * @return list handlers
   */
  List<FilterHandler<T>> getHandlers();

  /**
   * 跳转下一个流程
   *
   * @param t the t
   */
  @Override
  void forward(T t);
}
