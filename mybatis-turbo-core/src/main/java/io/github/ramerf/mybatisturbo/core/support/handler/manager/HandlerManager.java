package io.github.ramerf.mybatisturbo.core.support.handler.manager;

/**
 * 处理动作管理器.
 *
 * @param <T> the type parameter
 * @author zhairuihao
 * @since 2020 /4/3/003
 */
public interface HandlerManager<T> {

  /**
   * Forward.控制处理器流转
   *
   * @param t the t
   */
  void forward(T t);
}
