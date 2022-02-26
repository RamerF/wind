package io.github.ramerf.wind.core.executor.logging;

import javax.annotation.Nonnull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type Simple log.
 *
 * @since 2022.02.26
 * @author ramer
 */
public class SimpleLog implements Log {
  private static Logger log;

  public SimpleLog(@Nonnull final Class<?> clazz) {
    this(clazz.getName());
  }

  public SimpleLog(final String name) {
    log = LoggerFactory.getLogger(name);
  }

  @Override
  public boolean isDebugEnabled() {
    return log.isDebugEnabled();
  }

  @Override
  public boolean isTraceEnabled() {
    return log.isTraceEnabled();
  }

  @Override
  public void error(final String msg, final Throwable e) {
    log.error(msg, e);
  }

  @Override
  public void error(final String msg) {
    log.error(msg);
  }

  @Override
  public void debug(final String msg) {
    log.debug(msg);
  }

  @Override
  public void trace(final String msg) {
    log.trace(msg);
  }

  @Override
  public void warn(final String msg) {
    log.warn(msg);
  }
}
