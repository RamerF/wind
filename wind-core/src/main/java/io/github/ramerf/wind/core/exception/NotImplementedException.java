package io.github.ramerf.wind.core.exception;

/**
 * 方法未实现.
 *
 * @since 2021.01.31
 * @author ramer
 */
public class NotImplementedException extends RuntimeException {

  public NotImplementedException(String method) {
    super(method);
  }

  @Override
  public String toString() {
    return "Method " + getMessage() + " not implemented yet";
  }
}
