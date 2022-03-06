package io.github.ramerf.wind.core.plugin;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Invocation {

  private final Object target;
  private final Method method;
  private final Object[] args;
  /** 数据库操作类型. */
  private final ExecType execType;

  public Invocation(Object target, Method method, Object[] args) {
    this.target = target;
    this.method = method;
    this.args = args;
    this.execType =
        Plugins.QUERY_METHODS.contains(method.getName()) ? ExecType.READ : ExecType.WRITE;
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

  public Object proceed() throws InvocationTargetException, IllegalAccessException {
    return method.invoke(target, args);
  }

  /** 数据库操作类型. */
  public enum ExecType {
    READ,
    WRITE
  }
}
