package io.github.ramerf.wind.core.executor;

/**
 * The type Executor exception.
 *
 * @since 2022.02.20
 * @author ramer
 */
public class ExecutorException extends DataAccessException {

  public ExecutorException(String message) {
    super(message);
  }

  public ExecutorException(String message, Throwable cause) {
    super(message, cause);
  }
}
