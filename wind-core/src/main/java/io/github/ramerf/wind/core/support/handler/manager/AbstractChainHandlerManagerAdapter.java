package io.github.ramerf.wind.core.support.handler.manager;

import io.github.ramerf.wind.core.exception.CommonException;
import io.github.ramerf.wind.core.support.handler.FilterHandler;
import java.util.List;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;

import static io.github.ramerf.wind.core.entity.response.ResultCode.API_NOT_IMPLEMENT;

/**
 * 默认链式处理管理器实现.
 *
 * @author zhairuihao
 * @since 2020/4/8/008
 */
@Slf4j
public abstract class AbstractChainHandlerManagerAdapter<T> implements ChainHandlerManager<T> {
  /** 初始化next值 */
  private ThreadLocal<Integer> next = ThreadLocal.withInitial(() -> 0);

  @Override
  public void forward(T t) {
    int index = Optional.ofNullable(next.get()).orElse(0);
    if (log.isDebugEnabled()) {
      log.debug(
          " AbstractChainHandlerManagerAdapter.handle : [index:{},handlers:{}]",
          index,
          getHandlers());
    }

    if (index >= getHandlers().size()) {
      next.remove();
      return;
    }
    FilterHandler<T> handler = getHandlers().get(index);
    next.set(++index);
    try {
      handler.handle(t, this);
    } finally {
      next.remove();
    }
  }

  @Override
  public List<FilterHandler<T>> getHandlers() {
    throw CommonException.of(API_NOT_IMPLEMENT);
  }
}
