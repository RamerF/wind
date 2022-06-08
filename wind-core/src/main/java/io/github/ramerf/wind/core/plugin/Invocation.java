package io.github.ramerf.wind.core.plugin;

import io.github.ramerf.wind.core.reflect.ExceptionUtil;
import java.lang.reflect.Method;

public class Invocation {
  /** 被代理的目标对象. */
  private final Object target;

  /** 被代理的方法. */
  private final Method method;

  /** 被代理的方法参数. */
  private final Object[] args;
  /** 数据库操作类型. */
  private final ExecType execType;

  private final InterceptorChain interceptorChain;

  public Invocation(
      Object target, Method method, Object[] args, InterceptorChain interceptorChain) {
    this.target = target;
    this.method = method;
    this.args = args;
    this.execType =
        Plugins.QUERY_METHODS_DAO.contains(method.getName())
                || Plugins.QUERY_METHODS_SERVICE.contains(method.getName())
            ? ExecType.READ
            : ExecType.WRITE;
    this.interceptorChain = interceptorChain;
  }

  public Object getTarget() {
    return target;
  }

  public Method getMethod() {
    return method;
  }

  public Object[] getArgs() {
    return args;
  }

  public ExecType getExecType() {
    return execType;
  }

  /** 是否读数据库. */
  public boolean isReadMethod() {
    return execType.equals(ExecType.READ);
  }

  /** 是否写数据库. */
  public boolean isWriteMethod() {
    return execType.equals(ExecType.WRITE);
  }

  public Object proceed() throws Throwable {
    return interceptorChain.proceed(this);
  }

  Object invoke() throws Throwable {
    interceptorChain.reset();
    try {
      return method.invoke(target, args);
    } catch (Exception e) {
      throw ExceptionUtil.unwrapThrowable(e);
    }
  }

  /** 数据库操作类型. */
  public enum ExecType {
    READ,
    WRITE
  }
}
