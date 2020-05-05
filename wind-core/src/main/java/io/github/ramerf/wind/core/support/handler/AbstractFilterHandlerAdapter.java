package io.github.ramerf.wind.core.support.handler;

import io.github.ramerf.wind.core.support.handler.manager.HandlerManager;

/**
 * 处理器包装类，提供默认实现.
 *
 * @param <T> the type parameter
 * @author zhairuihao
 * @since 2020 /4/3/003
 */
public abstract class AbstractFilterHandlerAdapter<T> implements FilterHandler<T> {
  @Override
  public void handle(T t, HandlerManager<T> discountManager) {
    before(t);
    discountManager.forward(t);
    after(t);
  }

  /**
   * After.
   *
   * @param t the t
   */
  protected void after(T t) {}

  /**
   * Before.
   *
   * @param t the t
   */
  protected void before(T t) {}

  @Override
  public String toString() {
    return "AbstractHandlerAdapter";
  }
}
