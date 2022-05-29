package io.github.ramerf.wind.core.exception;

/**
 * 方法未实现.
 *
 * @since 2021.01.31
 * @author ramer
 */
public class NotImplementedException extends WindException {

  public NotImplementedException(String method) {
    super("Method " + method + " not implemented yet");
  }
}
