package io.github.ramerf.wind.core.event;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEvent;

/**
 * @author ramer
 * @since 13/08/2020
 */
@Slf4j
public class InitEvent extends ApplicationEvent {
  private final Object data;

  public Object getData() {
    return data;
  }

  /**
   * Create a new ApplicationEvent.
   *
   * @param source the object on which the event initially occurred (never {@code null})
   */
  public InitEvent(final Object source) {
    super(source);
    this.data = source;
    log.info("InitEvent:=================================================[{}]", data);
  }
}
