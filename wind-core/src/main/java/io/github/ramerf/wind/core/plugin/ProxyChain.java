package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.executor.DataAccessException;
import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Deque;
import lombok.Getter;
import net.sf.cglib.proxy.MethodProxy;

/**
 * @author ramer
 * @since 04/04/2022
 */
public class ProxyChain {
  @Getter private final Object target;
  private final Deque<ProxyChainPoint> points;
  private int index;

  public ProxyChain(final Object target, final Deque<ProxyChainPoint> points) {
    this.target = target;
    this.points = points;
  }

  public Object proceed(
      final Object proxyObj,
      final Method method,
      final Object[] args,
      final MethodProxy methodProxy) {
    if (points.isEmpty()) {
      try {
        return methodProxy.invokeSuper(proxyObj, args);
      } catch (IllegalAccessException | InvocationTargetException e) {
        throw new DataAccessException(ExceptionUtil.unwrapThrowable(e));
      } catch (Throwable e) {
        throw new RuntimeException(e);
      }
    } else {
      return points.poll().proceed(proxyObj, this, method, args, methodProxy);
    }
  }

  @FunctionalInterface
  public interface ProxyChainPoint {
    Object proceed(
        final Object proxyObj,
        final ProxyChain chain,
        final Method method,
        final Object[] args,
        final MethodProxy methodProxy);
  }
}
