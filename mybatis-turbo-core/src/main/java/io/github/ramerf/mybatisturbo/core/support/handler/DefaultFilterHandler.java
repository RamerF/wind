package io.github.ramerf.mybatisturbo.core.support.handler;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * 可以通过@order 注解指定调用顺序.
 *
 * @author zhairuihao
 * @since 2020 /4/4
 */
@Slf4j
@Service
public class DefaultFilterHandler<T> extends AbstractFilterHandlerAdapter<T> {
  @Override
  protected void before(T t) {
    if (log.isDebugEnabled()) {
      log.debug("DefaultOrderHandler.before");
    }
  }

  @Override
  protected void after(T t) {
    if (log.isDebugEnabled()) {
      log.debug("DefaultOrderHandler.after");
    }
  }
}
